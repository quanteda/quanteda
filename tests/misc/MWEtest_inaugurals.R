########################################################################################################
# Tests of statistics for detecting multiword expressions
# JK, 18.7.2017
# 
# Two functions: One for counting the expressions and one for calculating the statistics

# ************************************************************8
loglin_local <- function (table, margin, start = rep(1, length(table)), fit = FALSE, 
          eps = 0.1, iter = 20L, param = FALSE, print = TRUE) 
{
    rfit <- fit
    dtab <- dim(table)
    nvar <- length(dtab)
    ncon <- length(margin)
    conf <- matrix(0L, nrow = nvar, ncol = ncon)
    nmar <- 0
    varnames <- names(dimnames(table))
    for (k in seq_along(margin)) {
        tmp <- margin[[k]]
        if (is.character(tmp)) {
            tmp <- match(tmp, varnames)
            margin[[k]] <- tmp
        }
        if (!is.numeric(tmp) || any(is.na(tmp) | tmp <= 0)) 
            stop("'margin' must contain names or numbers corresponding to 'table'")
        conf[seq_along(tmp), k] <- tmp
        nmar <- nmar + prod(dtab[tmp])
    }
    ntab <- length(table)
    if (length(start) != ntab) 
        stop("'start' and 'table' must be same length")
    z <- .Call(stats:::C_LogLin, dtab, conf, table, start, nmar, eps, 
               iter)
    if (print) 
        cat(z$nlast, "iterations: deviation", z$dev[z$nlast], 
            "\\n")
    fit <- z$fit
    attributes(fit) <- attributes(table)
    observed <- as.vector(table[start > 0])
    expected <- as.vector(fit[start > 0])
    pearson <- sum((observed - expected)^2/expected)
    observed <- as.vector(table[table * fit > 0])
    expected <- as.vector(fit[table * fit > 0])
    lrt <- 2 * sum(observed * log(observed/expected))
    subsets <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }
    df <- rep.int(0, 2^nvar)
    for (k in seq_along(margin)) {
        terms <- subsets(margin[[k]])
        for (j in seq_along(terms)) df[sum(2^(terms[[j]] - 1))] <- prod(dtab[terms[[j]]] - 
                                                                            1)
    }
    if (!is.null(varnames) && all(nzchar(varnames))) {
        for (k in seq_along(margin)) margin[[k]] <- varnames[margin[[k]]]
    }
    else {
        varnames <- as.character(1:ntab)
    }
    y <- list(lrt = lrt, pearson = pearson, df = ntab - sum(df) - 
                  1, margin = margin)
    if (rfit) 
        y$fit <- fit
    if (param) {
        fit <- log(fit)
        terms <- seq_along(df)[df > 0]
        parlen <- length(terms) + 1
        parval <- list(parlen)
        parnam <- character(parlen)
        parval[[1L]] <- mean(fit)
        parnam[1L] <- "(Intercept)"
        fit <- fit - parval[[1L]]
        dyadic <- NULL
        while (any(terms > 0)) {
            dyadic <- cbind(dyadic, terms%%2)
            terms <- terms%/%2
        }
        dyadic <- dyadic[order(rowSums(dyadic)), , drop = FALSE]
        for (i in 2:parlen) {
            vars <- which(dyadic[i - 1, ] > 0)
            parval[[i]] <- apply(fit, vars, mean)
            parnam[i] <- paste(varnames[vars], collapse = ".")
            fit <- sweep(fit, vars, parval[[i]], check.margin = FALSE)
        }
        names(parval) <- parnam
        y$param <- parval
    }
    return(y)
}
MWEcounts <- function (candidate,text,stopword="xxxx") 
{
# Function for creating the 2^K table of yes/no occurrences 
# in text (character vector) 
# of words in a K-word candidate expression (character vector) 
#
	K <- length(candidate)
	J <- length(text)-K+1

##############################################################################
# Fixed objects, here up to candidate length of 4 (extend as needed)

	count.vectors <- list(
		c("00","01","10","11")
	)
	count.vectors[[2]] <- paste(rep(c("0","1"),each=4), rep(count.vectors[[1]],2),sep="")
	count.vectors[[3]] <- paste(rep(c("0","1"),each=8), rep(count.vectors[[2]],2),sep="")
#
	noyes <- c("no","yes")
	array.dims <- list(
		list(W2=noyes,W1=noyes),
		list(W3=noyes,W2=noyes,W1=noyes),
		list(W4=noyes,W3=noyes,W2=noyes,W1=noyes)
	)
#
	data.frames <- list(
		data.frame(count=NA,W1=gl(2,2,4,labels=noyes),W2=gl(2,1,4,labels=noyes)),
		data.frame(count=NA,W1=gl(2,4,8,labels=noyes),W2=gl(2,2,8,labels=noyes),W3=gl(2,1,8,labels=noyes)),
		data.frame(count=NA,W1=gl(2,8,16,labels=noyes),W2=gl(2,4,16,labels=noyes),W3=gl(2,2,16,labels=noyes),W4=gl(2,1,16,labels=noyes))
	) 

###############################################################################
# Count the words

	counts <- rep(0,2^K)
	names(counts) <- count.vectors[[K-1]]
#
	for(j in seq(J)){
		text.j <- text[j:(j+K-1)]
		if(all(text.j!=stopword)){
			agreement <- text.j==candidate
			tmp <- paste(as.numeric(agreement),collapse="")
			counts[tmp] <- counts[tmp]+1
		}
	}	
	counts.table <- array(counts,dim=rep(2,K),dimnames=array.dims[[K-1]])
	counts.data.frame <- data.frames[[K-1]]
	counts.data.frame$count <- counts
#
	result <- list(expression=paste(candidate,collapse=" "),counts=counts,counts.table=counts.table,counts.data.frame=counts.data.frame)
	return(result)
}

# ************************************************************8

MWEstatistics <- function (counts,smooth=0.5) 
{
# Function for calculating some association statistics for a 
# K-word candidate expression 
# The input is output from the function MWEcounts 
#
	counts.n <- counts$counts
	counts.table <- counts$counts.table
	counts.df <- counts$counts.data.frame
	K <- length(dim(counts.table))

	results <- matrix(NA,1,9+(2^K))
	colnames(results) <- c("length","lambda","se.lambda","z.lambda","LRtest","smooth","mInfty","Infty","N",names(counts.n))
	rownames(results) <- counts$expression
	results[,"length"] <- K
	results[,"smooth"] <- smooth
	results[,-(1:9)] <- counts.n
	results[,"N"] <- sum(counts.n)
	results[,"mInfty"] <- as.numeric(counts.n[2^K]==0) # 1 if the expression never appears in the text
	results[,"Infty"] <- as.numeric(any(counts.n[-(2^K)]==0)) # 1 if the count for any lower-order margin is 0 (i.e. the log-OR is infinity)

##############################################################################
# Fixed objects, here up to candidate length of 4 (extend as needed)

	loglin.margins <- list(
		list(1,2),
		list(1:2,2:3,c(1,3)),
		list(1:3,2:4,c(1,2,4),c(1,3,4))
	)[[K-1]]
	formula <- list(
		count~W1*W2,
		count~W1*W2*W3,
		count~W1*W2*W3*W4
	)[[K-1]]

###############################################################################
# Estimated highest-order interaction parameter (lambda), obtained using a Poisson log-linear model

	counts.df$count <- counts.df$count+smooth
	options(warn=-1) # Switch of the warning due to the non-integer counts 
	mod1 <- glm(formula,family=poisson,data=counts.df)
	options(warn=0)
	tmp <- length(coef(mod1))
	results[,"lambda"] <- coef(mod1)[tmp]
	results[,"se.lambda"] <- sqrt(diag(vcov(mod1)))[tmp]
	results[,"z.lambda"] <- results[,"lambda"]/results[,"se.lambda"]

# Likelihood ratio test of the parameter, obtained from an IPF fit from the loglin function for model without the highest-order interaction
# (note: this could also be obtained by fitting this model and the saturated model with glm, and asking for the LR test

	counts.table <- counts.table+smooth
	mod2 <- loglin_local(counts.table,loglin.margins,print=F)	
	results[,"LRtest"] <- mod2$lrt
#
	return(results)
}

#######################################################################################################################
#######################################################################################################################

# Loading and processing example corpus

# load("inaugTexts.RData")
# inaugTexts.vector <- unlist(sapply(inaugTexts,FUN=function(x){strsplit(x, " ")}))
# names(inaugTexts.vector) <- NULL
# 
# class(inaugTexts.vector)
# length(inaugTexts.vector)
# inaugTexts.vector[1:1000]
# t.tmp <- inaugTexts.vector
# 
# inaugTexts.vector <- gsub("\"","",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub("(","",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub(")","",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub(".\n\n","AAAXXXXAAA",inaugTexts.vector,fixed=T) # xxxx will represent any stopword 
# inaugTexts.vector <- gsub(":\n\n","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub("\n","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub(",","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub(".","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub("--","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- gsub(";","AAAXXXXAAA",inaugTexts.vector,fixed=T)
# inaugTexts.vector <- unlist(strsplit(inaugTexts.vector,"AAA"))
# inaugTexts.vector <- inaugTexts.vector[inaugTexts.vector!=""]
# inaugTexts.vector <- tolower(inaugTexts.vector)
# 
# # eyeball check:
# cbind(t.tmp[1:500],inaugTexts.vector[1:500])

##############################################################################
# Tests with a few candidate expressions
## counts:

# test2.tmp <- MWEcounts(c("united","states"),inaugTexts.vector)
# test3.tmp <- MWEcounts(c("house","of","representatives"),inaugTexts.vector)
# test4.tmp <- MWEcounts(c("united","states","of","america"),inaugTexts.vector)
# 
# test2.tmp
# test3.tmp
# test4.tmp
# 
# MWEstatistics(test2.tmp)
# MWEstatistics(test3.tmp)
# MWEstatistics(test4.tmp)
# 
# #MWEstatistics(test2.tmp,smooth=0)
# MWEstatistics(test3.tmp,smooth=0)
# MWEstatistics(test4.tmp,smooth=0)
# 
# # Some two-word expressions:
# 
# mwe2.examples <- MWEstatistics(MWEcounts(c("united","states"),inaugTexts.vector))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("supreme","court"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("american","people"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("federal","government"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("national","government"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("american","dream"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("george","washington"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("vice","president"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("our","people"),inaugTexts.vector))) # Example of a relatively low odds ratio (but high test statistics, because of larger counts)
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("middle","east"),inaugTexts.vector))) # Both words are rare, so their appearance together (although also rare) gives high odds ratio
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("we","will"),inaugTexts.vector))) # Both words are common; association is moderate but highly significant
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("united","nations"),inaugTexts.vector)))
# mwe2.examples <- rbind(mwe2.examples,MWEstatistics(MWEcounts(c("two","centuries"),inaugTexts.vector)))
# 
# mwe2.examples
# 
# mwe3.examples <- MWEstatistics(MWEcounts(c("house","of","representatives"),inaugTexts.vector))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("bill","of","rights"),inaugTexts.vector)))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("declaration","of","independence"),inaugTexts.vector)))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("way","of","life"),inaugTexts.vector)))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("men","and","women"),inaugTexts.vector)))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("they","may","be"),inaugTexts.vector))) # Three-word expression which is *less* common than we would expect based on the pairwise distributions
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("we","the","people"),inaugTexts.vector)))
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("people","of","the"),inaugTexts.vector))) # Looks like "people of" is followed by many different things, not particularly often by "the.."
# mwe3.examples <- rbind(mwe3.examples,MWEstatistics(MWEcounts(c("we","do","not"),inaugTexts.vector))) # common words, even in combination, but not more so in combination than we would expect
# 
# mwe3.examples


####*****************quanteda tests***********************
toks <- tokens(inaugTexts, what = "fasterword")
toks <- tokens_remove(toks, "^\\p{P}$", valuetype = "regex", padding = TRUE)
toks <- tokens_tolower(toks)
inaugTexts.vector <- as.character(toks)

### smoothing = 0.5
seqs <- textstat_collocations(toks, size=2:4)

## bigram comparison
seq2 <- seqs[seqs$collocation == "united states",]
test2.tmp <- MWEcounts(c("united","states"),inaugTexts.vector)
test2_stat <- MWEstatistics(test2.tmp)
expect_equal(seq2$lambda, test2_stat[2], tolerance =0.01)
expect_equal(seq2$sigma, test2_stat[3], tolerance =0.01)
expect_equal(seq2$z, test2_stat[4], tolerance =0.01)

##trigram comparison
test3.tmp <- MWEcounts(c("house","of","representatives"),inaugTexts.vector)
test3_stat <- MWEstatistics(test3.tmp)
seq3 <- seqs[seqs$collocation == "house of representatives",]
expect_equal(seq3$lambda, test3_stat[2], tolerance =0.01)
expect_equal(seq3$sigma, test3_stat[3], tolerance =0.01)
expect_equal(seq3$z, test3_stat[4], tolerance =0.01)

### ?? "united states of america"  0  occurance
test4.tmp <- MWEcounts(c("united","states","of","america"),inaugTexts.vector)
test4_stat <- MWEstatistics(test4.tmp)
seq4 <- seqs[seqs$collocation == "united states of america",]
expect_equal(seq4$lambda, test4_stat[2], tolerance =0.01)
expect_equal(seq4$sigma, test4_stat[3], tolerance =0.01)
expect_equal(seq4$z, test4_stat[4], tolerance =0.01)

# 4- gram comparison
test4.tmp <- MWEcounts(c("on","the","part","of"),inaugTexts.vector)
test4_stat <- MWEstatistics(test4.tmp)
seq4 <- seqs[seqs$collocation == "on the part of",]
expect_equal(seq4$lambda, test4_stat[2], tolerance =0.01)
expect_equal(seq4$sigma, test4_stat[3], tolerance =0.01)
expect_equal(seq4$z, test4_stat[4], tolerance =0.01)

### smoothing = 0.0
#seqs <- textstat_collocations(toks, size=2:4, smoothing = 0)
seqs <- sequences(toks, size = 2:4, smoothing = 0)

## bigram comparison
seq2 <- seqs[seqs$collocation == "united states",]
test2.tmp <- MWEcounts(c("united","states"),inaugTexts.vector)
test2_stat <- MWEstatistics(test2.tmp, smooth = 0)
expect_equal(seq2$lambda, test2_stat[2], tolerance =0.01)
expect_equal(seq2$sigma, test2_stat[3], tolerance =0.01)
expect_equal(seq2$z, test2_stat[4], tolerance =0.01)

##trigram comparison
## collocation returns Inf for lambda without smoothing -- because of zero countings (log0)
test3.tmp <- MWEcounts(c("house","of","representatives"),inaugTexts.vector)
test3_stat <- MWEstatistics(test3.tmp, smooth = 0)
seq3 <- seqs[seqs$collocation == "house of representatives",]
expect_equal(seq3$lambda, test3_stat[2], tolerance =0.01)
expect_equal(seq3$sigma, test3_stat[3], tolerance =0.01)
expect_equal(seq3$z, test3_stat[4], tolerance =0.01)

# 4- gram comparison
## collocation returns Inf for lambda without smoothing -- because of zero countings (log0)
test4.tmp <- MWEcounts(c("on","the","part","of"),inaugTexts.vector)
test4_stat <- MWEstatistics(test4.tmp, smooth = 0)
seq4 <- seqs[seqs$collocation == "on the part of",]
expect_equal(seq4$lambda, test4_stat[2], tolerance =0.01)
expect_equal(seq4$sigma, test4_stat[3], tolerance =0.01)
expect_equal(seq4$z, test4_stat[4], tolerance =0.01)
