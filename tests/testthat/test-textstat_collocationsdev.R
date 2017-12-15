context('test textstat_collocationsdev.R')
########################################################################################################
# Tests of statistics for detecting multiword expressions
# JK, 18.7.2017
# 
# Two functions: One for counting the expressions and one for calculating the statistics

# ************************************************************

MWEcounts <- function (candidate,text,stopword="xxxx") {
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

MWEstatistics <- function (counts, smooth=0.5) {
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

test_that("test that collocations do not span texts", {
    toks <- tokens(c('this is a test', 'this also a test'))
    cols <- rbind(textstat_collocationsdev(toks, size = 2, min_count = 1),
                  textstat_collocationsdev(toks, size = 3, min_count = 1))
    
    expect_false('test this' %in% cols$collocation)
    expect_false('test this also' %in% cols$collocation)
    expect_true('this also a' %in% cols$collocation)
})

test_that("test that collocations only include selected features", {
    toks <- tokens(c('This is a Twitter post to @someone on #something.'), what = 'fastest')
    toks <- tokens_select(toks, "^([a-z]+)$", valuetype = "regex")
    cols <- textstat_collocationsdev(toks, min_count = 1, size = 2, tolower = FALSE)
    
    expect_true('This is' %in% cols$collocation)
    expect_true('a Twitter' %in% cols$collocation)
    
    expect_false('to @someone' %in% cols$collocation)
    expect_false('on #something' %in% cols$collocation)
})

# test_that("test that collocations and sequences are counting the same features", {
#     toks <- tokens(data_corpus_inaugural[1:2], remove_punct = TRUE)
#     toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
#     seqs <- textstat_collocationsdev(toks, method = 'lambda', size = 2)
#     cols <- textstat_collocationsdev(toks, method = 'lambda1', size = 2)  # now is equal to `lambda`
#     both <- merge(seqs, cols, by = 'collocation')
#     expect_true(all(both$count.x == both$count.y))
# })

# test_that("test that extractor works with collocation", {
#     
#     toks <- tokens(data_corpus_inaugural, remove_punct = TRUE)
#     toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#     cols <- textstat_collocationsdev(toks, method = 'lr', size = 2)
#     cols <- cols[1:5,]
#     expect_equal(nrow(cols), length(as.tokens(cols)))
#     
# })

test_that("bigrams and trigrams are all sorted correctly, issue #385", {
    toks <- tokens(data_corpus_inaugural[2], remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
    cols <- textstat_collocationsdev(toks, method = 'lambda', min_count = 1, size = 2:3)
    expect_equal(order(cols$z, decreasing = TRUE), seq_len(nrow(cols)))
})

test_that("test the correctness of significant with smoothing", {
    toks <- tokens('capital other capital gains other capital word2 other gains capital')
    seqs <- textstat_collocationsdev(toks, min_count=1, size = 2)
    # smoothing is applied when calculating the dice, so the dice coefficient 
    #is only tested against manually calculated result.
    
    expect_equal(seqs$collocation[1], 'other capital')
    
    #dice
    # expect_equal(seqs$dice[1], 0.625)
    
    #pmi
    # expect_equal(seqs$pmi[1], log(2.5*(9+0.5*4)/(2.5+1.5)/(2.5+1.5)))
})

test_that("test the correctness of significant", {
    toks <- tokens('capital other capital gains other capital word2 other gains capital')
    seqs <- textstat_collocationsdev(toks, min_count=1, size = 2, smoothing = 0)
    
    expect_equal(seqs$collocation[1], 'other capital')
    
    # #dice
    # # expect_equal(seqs$dice[1], 0.667, tolerance = 1e-3)
    # 
    # #pmi
    # expect_equal(seqs$pmi[1], log2(2*9/(2+1)/(2+1)))
    # 
    # #chi2
    # expect_equal(seqs$chi2[1], 2.25)
    # 
    # #log likelihood ratio
    # expect_equal(seqs$G2[1], 2.231, tolerance = 1e-3)
})


test_that("textstat_collocationsdev works with corpus, character, tokens objects", {
    txt <- data_char_sampletext
    corp <- corpus(txt)
    expect_equal(
        textstat_collocationsdev(txt, min_count = 2, size = 3),
        textstat_collocationsdev(corp, min_count = 2, size = 3)
    )
    # expect_equal(
    #     textstat_collocationsdev(tokens(txt), min_count = 2, size = 3),
    #     textstat_collocationsdev(tokens(txt, hash = FALSE), min_count = 2, size = 3)
    # )
    # 
    # ## THIS SHOULD BE THE SAME, BUT IS NOT BECAUSE THE REMOVED PUNCTUATION BECOMES
    # ## PADS, AND IS COUNTED, WHEN IT SHOULD NOT BE COUNTED AT ALL
    # 
    # toks <- tokens(txt)
    # seqs_corp <- textstat_collocationsdev(corp, method = "lambda", min_count = 2, size = 3)
    # seqs_toks <- textstat_collocationsdev(tokens_remove(toks, "\\p{P}", valuetype = "regex", padding = TRUE), method = "lambda", min_count = 2, size = 3)
    # expect_equal(
    #     seqs_corp[, 1:3],
    #     seqs_toks[match(seqs_corp$collocation, seqs_toks$collocation), 1:3],
    #     check.attributes = FALSE
    # )
})

test_that("lambda & [ function",{
    toks <- tokens('E E G F a b c E E G G f E E f f G G')
    toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                                  case_insensitive = FALSE, padding = TRUE)
    seqs <- textstat_collocationsdev(toks_capital, min_count = 1, show_counts = TRUE)
    a_seq <- seqs[1, ]
    
    #call Jouni's implementation
    tt <- as.character(toks_capital)
    test2 <- MWEcounts(c("G", "F"), tt)
    test2_stat <- suppressWarnings(MWEstatistics(test2))
    
    expect_equal(a_seq$collocation, 'g f')
    expect_equal(a_seq$lambda, test2_stat[2])
    expect_equal(class(a_seq), c("collocationsdev", 'data.frame'))
    expect_equal(a_seq$n00, test2_stat[10]+0.5)
    expect_equal(a_seq$n11, test2_stat[13]+0.5)
})

# test_that("deprecated collocations function works", {
#     txts <- data_corpus_inaugural[1:2]
#     expect_equal(
#         suppressWarnings(collocations(txts, size = 2, min_count = 2)),
#         textstat_collocationsdev(txts, size = 2, min_count = 2)
#     )
#     expect_warning(
#         collocations(txts, size = 2, min_count = 2),
#         "'collocations' is deprecated"
#     )
#     expect_warning(
#         sequences(txts, size = 2, min_count = 2),
#         "'sequences' is deprecated"
#     )
# })

test_that("textstat_collocationsdev.tokens works ok with zero-length documents (#940)", {
    txt <- c('I like good ice cream.', 'Me too!  I like good ice cream.', '')
    toks <- tokens(tolower(txt), remove_punct = TRUE, remove_symbols = TRUE)
    
    expect_equal(
        textstat_collocationsdev(toks, size = 2, min_count = 2, tolower = TRUE)$collocation,
        c("ice cream", "like good", "i like", "good ice")
    )
    ##   collocation count length   lambda        z
    ## 1   ice cream     2      2 4.317488 2.027787
    ## 2   like good     2      2 4.317488 2.027787
    ## 3      i like     2      2 4.317488 2.027787
    ## 4    good ice     2      2 4.317488 2.027787
    
    expect_equal(
        textstat_collocationsdev(toks, size = 2, min_count = 2)$collocation,
        c("ice cream", "like good", "i like", "good ice")
    )
})

test_that("textstat_collocationsdev works when texts are shorter than size", {
    toks <- tokens(c('a', 'bb', ''))
    expect_equivalent(
        textstat_collocationsdev(toks, size = 2:3, min_count = 1),
        data.frame(collocation = character(0),
                   count = integer(0),
                   length = numeric(0),
                   lambda = numeric(0),
                   z = numeric(0),
                   G2 = numeric(0),
                   chi2 = numeric(0),
                   pmi = numeric(0),
                   LFMD = numeric(0),
                   stringsAsFactors = FALSE))
})

test_that("textstat_collocationsdev error when size = 1 and warn when size > 5", {
    
    toks <- tokens('a b c d e f g h a b c d e f')
    expect_silent(textstat_collocationsdev(toks, size = 2:5))
    expect_error(textstat_collocationsdev(toks, size = 1:5),
                 "Collocation sizes must be larger than 1")
    expect_error(textstat_collocationsdev(toks, size = 2:6),
                   "Collocation sizes must be smaller than 6")
    
})
