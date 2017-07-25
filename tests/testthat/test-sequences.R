context('test sequences.R')

########################################################################################################
# Tests of statistics for detecting multiword expressions
# JK, 18.7.2017
# 
# Two functions: One for counting the expressions and one for calculating the statistics

# ************************************************************8

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

MWEstatistics <- function (counts, smooth=0.5) 
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
    mod2 <- loglin(counts.table,loglin.margins,print=F)	
    results[,"LRtest"] <- mod2$lrt
    #
    return(results)
}

test_that("test that argument 'size'", {
    
    toks <- tokens('E E a b c E E G G G f E E f f G G')
    toks <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                          case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks, min_count = 1)
    expect_equal(seqs$collocation, c('G G', 'E E', 'E G'))
    
    seqs <- sequences(toks, min_count = 1, size=3:4, method = "lambda1")
    expect_equal(seqs$collocation, c('G G G', 'E E G G', 'E G G G', 'E G G', 'E E G'))
})

test_that("test that argument 'method'", {
    
    toks <- tokens('c o c g o c w o g c')
    
    #the largest count is: counts(NOTcNOTgNOTo)=4; and it has a positive sign in "lambda1"
    seqs <- sequences(toks, min_count = 1, method = "lambda1", size = 3)
    cgo_index <- which(seqs$collocation == "c g o")
    expect_gt(seqs$lambda[cgo_index], 0)              
    
    #the largest count is: counts(NOTcNOTgNOTo)=4; and it has a negtive sign in "lambda"
    seqs <- sequences(toks, min_count = 1, method = "lambda", size = 3)
    cgo_index <- which(seqs$collocation == "c g o")
    expect_lt(seqs$lambda[cgo_index], 0)
    
})

# test_that("test that sequences works with tokens_compound", {
#     
#     toks <- tokens('E E a b c E E G G f E E f f G G')
#     toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
#                           case_insensitive = FALSE, padding = TRUE)
#     seqs <- sequences(toks_capital, min_count = 1, size = 2:4)
#     
#     # THE NEXT TWO TESTS HIGHLIGHT THE ISSUES RELATING TO NESTED SEQUENCES.
#     # MAYBE WE SHOULD HAVE A RULE TO FORM THE LONGEST ONLY?  -KB
# 
#     # seqs have the same types
#     expect_equivalent(as.list(tokens_compound(toks, phrase(seqs), join = FALSE)),
#                       list(c("E_E", "a", "b", "c", "E_E_G_G", "E_E", "G_G", "f", "E_E", "f", "f", "G_G")))
#     
#     # seqs have different types
#     attr(seqs, 'types') <- ''
#     expect_equivalent(as.list(tokens_compound(toks, phrase(seqs), join = FALSE)),
#                       list(c("E_E", "a", "b", "c", "E_E_G_G", "E_E", "G_G", "f", "E_E", "f", "f", "G_G")))
#     
# })

test_that("[ function",{
    toks <- tokens('E E G F a b c E E G G f E E f f G G')
    toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                                  case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks_capital, min_count = 1)
    a_seq <- seqs[1]
    
    #call Jouni's implementation
    tt <- as.character(toks_capital)
    test2 <- MWEcounts(c("G", "F"), tt)
    test2_stat <- suppressWarnings(MWEstatistics(test2))

    expect_equal(a_seq$collocation, 'G F')
    expect_equal(a_seq$lambda, test2_stat[2])
    expect_equal(class(a_seq), c("sequences", 'data.frame'))
})

test_that("is.sequences function",{
    toks <- tokens('E E a b c E E G G f E E f f G G')
    toks <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                          case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks, min_count = 1)
    
    expect_false(is.sequences(toks))
    expect_true(is.sequences(seqs))
})

test_that("test the correctness of significant with smoothing", {
     toks <- tokens('capital other capital gains other capital word2 other gains capital')
     seqs <- sequences(toks, min_count=1, size = 2)
    # smoothing is applied when calculating the dice, so the dice coefficient 
     #is only tested against manually calculated result.
     
     expect_equal(seqs$collocation[1], 'other capital')
     
     #dice
     expect_equal(seqs$dice[1], 0.625)
     
     #pmi
     expect_equal(seqs$pmi[1], log(2.5*(9+0.5*4)/(2.5+1.5)/(2.5+1.5)))
 })

test_that("test the correctness of significant", {
    toks <- tokens('capital other capital gains other capital word2 other gains capital')
    seqs <- sequences(toks, min_count=1, size = 2, smoothing = 0)

    expect_equal(seqs$collocation[1], 'other capital')
    
    #dice
    expect_equal(seqs$dice[1], 0.667, tolerance = 1e-3)
    
    #pmi
    expect_equal(seqs$pmi[1], log(2*9/(2+1)/(2+1)))
    
    #chi2
    expect_equal(seqs$chi2[1], 2.25)
    
    #log likelihood ratio
    expect_equal(seqs$G2[1], 2.231, tolerance = 1e-3)
})

test_that("test the correctness of significant: against stats package", {
    txt <- "A gains capital B C capital gains A B capital C capital gains tax gains tax gains B gains C capital gains tax"
    toks <- as.character(tokens(txt, ngrams = 3, concatenator = " "))

    require(data.table)
    toks_df <- data.table(do.call(rbind, strsplit(toks, " ")), stringsAsFactors = FALSE)
    names(toks_df) <- paste0("word", 1:3)

    # replace non-target words with "other"
    targets <- c("capital", "gains", "tax")
    toks_df[word1 != targets[1], word1 := "other"]
    toks_df[word2 != targets[2], word2 := "other"]
    toks_df[word3 != targets[3], word3 := "other"]
    toks_df[, n := 1]
    toks_df_n <- toks_df[, list(n = sum(n)), by = c("word1", "word2", "word3")]
    toks_df_n

    # test code for lr = lr(a, b, c)
    # statss <- stats::loglin(table(toks_df[, 1:3]), margin = 1:3)
    # # 2 iterations: deviation 1.776357e-15
    # # $lrt
    # # [1] 10.91587
    # #
    # # $pearson
    # # [1] 16.93125
    # #
    # # $df
    # # [1] 4
    # #
    # # $margin
    # # [1] "word1" NA      NA
    # 
    # seqs <- sequences(tokens(txt), size = 3, smoothing =0)
    # expect_equal(seqs$collocation[3], 'capital gains tax')
    # expect_equal(seqs$logratio[3], statss$lrt, tolerance = 1e-3)
    # expect_equal(seqs$chi2[3], statss$pearson, tolerance = 1e-3)
    # 
    # col <- textstat_collocations(tokens(txt), method = "lr", size = 3)
    # expect_equal(seqs$collocation[3], col$collocation[3])
    # expect_equal(seqs$logratio[3], col$G2[3], tolerance = 1e-3)
    # #         collocation length count       G2
    # # 1   C capital gains      3     3 20.17961
    # # 2   gains tax gains      3     2 11.18707
    # # 3 capital gains tax      3     2 10.91587
    # 
    # col <- textstat_collocations(tokens(txt), method = "chi2", size = 3)
    # expect_equal(seqs$chi2[3], col$X2[3], tolerance = 1e-3)
    # #         collocation length count       X2
    # # 1   C capital gains      3     3 43.95563
    # # 2   gains tax gains      3     2 23.64158
    # # 3 capital gains tax      3     2 16.93125
    # 
    # col <- textstat_collocations(tokens(txt), method = "pmi", size = 3)
    # expect_equal(seqs$pmi[3], col$pmi[3], tolerance = 1e-3)
    # #    collocation length count      pmi
    # # 1   C capital gains      3     3 2.687847
    # # 2   gains tax gains      3     2 2.505526
    # # 3 capital gains tax      3     2 2.128232
    # 
    # col <- textstat_collocations(tokens(txt), method = "dice", size = 3)
    # expect_equal(seqs$dice[3], col$dice[3], tolerance = 1e-3)
    # #     collocation length count      dice
    # # 1   C capital gains      3     3 0.4285714
    # # 2   gains tax gains      3     2 0.2857143
    # # 3 capital gains tax      3     2 0.2666667
    
    tt <- toks_df[, cg :=paste(word1,word2)]
    tt <- tt[cg !="capital gains", cg:="other"]
    toks_df_2x2 <- table(tt[,c(3,5)])
    statss <- stats::loglin(toks_df_2x2, margin=1:2)
    
    seqs <- sequences(tokens(txt), size = 3, smoothing = 0)
    cgt_index <- which(seqs$collocation == "capital gains tax")
    expect_equal(seqs$collocation[cgt_index], 'capital gains tax')
    expect_equal(seqs$G2[cgt_index], statss$lrt, tolerance = 1e-3)
    expect_equal(seqs$chi2[cgt_index], statss$pearson, tolerance = 1e-3)
})



test_that("test the correctness of lambda: against Jouni's", {
    toks <- tokens(inaugTexts, what = "fasterword")
    toks <- tokens_remove(toks, "^\\p{P}$", valuetype = "regex", padding = TRUE)
    toks <- tokens_tolower(toks)
    inaugTexts.vector <- as.character(toks)
    
    ### smoothing = 0.5
    seqs <- textstat_collocations(toks, size=2:4)
    
    ## bigram comparison
    seq2 <- seqs[seqs$collocation == "united states",]
    test2.tmp <- MWEcounts(c("united","states"),inaugTexts.vector)
    test2_stat <- suppressWarnings(MWEstatistics(test2.tmp))
    expect_equal(seq2$lambda, test2_stat[2], tolerance =0.01)
    expect_equal(seq2$sigma, test2_stat[3], tolerance =0.01)
    expect_equal(seq2$z, test2_stat[4], tolerance =0.01)
    
    ##trigram comparison
    test3.tmp <- MWEcounts(c("house","of","representatives"),inaugTexts.vector)
    test3_stat <-suppressWarnings(MWEstatistics(test3.tmp))
    seq3 <- seqs[seqs$collocation == "house of representatives",]
    expect_equal(seq3$lambda, test3_stat[2], tolerance =0.01)
    expect_equal(seq3$sigma, test3_stat[3], tolerance =0.01)
    expect_equal(seq3$z, test3_stat[4], tolerance =0.01)    
    
    # 4- gram comparison
    test4.tmp <- MWEcounts(c("on","the","part","of"),inaugTexts.vector)
    test4_stat <- suppressWarnings(MWEstatistics(test4.tmp))
    seq4 <- seqs[seqs$collocation == "on the part of",]
    expect_equal(seq4$lambda, test4_stat[2], tolerance =0.01)
    expect_equal(seq4$sigma, test4_stat[3], tolerance =0.01)
    expect_equal(seq4$z, test4_stat[4], tolerance =0.01)
})
    