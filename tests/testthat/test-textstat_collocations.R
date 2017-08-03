context('test textstat_collocations.R')

test_that("test that collocations do not span texts", {
    toks <- tokens(c('this is a test', 'this also a test'))
    cols <- rbind(textstat_collocations(toks, size = 2, min_count = 1),
                  textstat_collocations(toks, size = 3, min_count = 1))
    
    expect_false('test this' %in% cols$collocation)
    expect_false('test this also' %in% cols$collocation)
    expect_true('this also a' %in% cols$collocation)
})

test_that("test that collocations only include selected features", {
    toks <- tokens(c('This is a Twitter post to @someone on #something.'), what = 'fastest')
    toks <- tokens_select(toks, "^([a-z]+)$", valuetype = "regex")
    cols <- textstat_collocations(toks, min_count = 1, size = 2, tolower = FALSE)
    
    expect_true('This is' %in% cols$collocation)
    expect_true('a Twitter' %in% cols$collocation)
    
    expect_false('to @someone' %in% cols$collocation)
    expect_false('on #something' %in% cols$collocation)
})

test_that("test that collocations and sequences are counting the same features", {
    toks <- tokens(data_corpus_inaugural[1:2], remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
    seqs <- textstat_collocations(toks, method = 'lambda', size = 2)
    cols <- textstat_collocations(toks, method = 'lambda1', size = 2)  # now is equal to `lambda`
    both <- merge(seqs, cols, by = 'collocation')
    expect_true(all(both$count.x == both$count.y))
})

# test_that("test that extractor works with collocation", {
#     
#     toks <- tokens(data_corpus_inaugural, remove_punct = TRUE)
#     toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#     cols <- textstat_collocations(toks, method = 'lr', size = 2)
#     cols <- cols[1:5,]
#     expect_equal(nrow(cols), length(as.tokens(cols)))
#     
# })

test_that("bigrams and trigrams are all sorted correctly, issue #385", {
    toks <- tokens(data_corpus_inaugural[2], remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
    cols <- textstat_collocations(toks, method = 'lambda', size = 3)
    expect_equal(order(cols$z, decreasing = TRUE), seq_len(nrow(cols)))
})

test_that("test the correctness of significant with smoothing", {
    toks <- tokens('capital other capital gains other capital word2 other gains capital')
    seqs <- textstat_collocations(toks, min_count=1, size = 2)
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
    seqs <- textstat_collocations(toks, min_count=1, size = 2, smoothing = 0)
    
    expect_equal(seqs$collocation[1], 'other capital')
    
    #dice
    # expect_equal(seqs$dice[1], 0.667, tolerance = 1e-3)
    
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
    
    seqs <- textstat_collocations(tokens(txt), min_count = 2,  size = 3, smoothing = 0)
    expect_equal(seqs$collocation[3], 'capital gains tax')
    expect_equal(seqs$G2[3], statss$lrt, tolerance = 1e-3)
    expect_equal(seqs$chi2[3], statss$pearson, tolerance = 1e-3)
})

test_that("collocation is counted correctly in racing conditions, issue #381", {

    toks <- tokens(rep(texts(data_corpus_inaugural)[1], 2))
    out1 <- textstat_collocations(toks[1], size = 2, min_count = 1)
    out100 <- textstat_collocations(toks, size = 2, min_count = 1)
    out1 <- out1[order(out1$collocation),]
    out100 <- out100[order(out100$collocation),]
    # expect_true(all(out1$count * 100 == out100$count))

})

test_that("textstat_collocations works with corpus, character, tokens objects", {
    txt <- data_char_sampletext
    corp <- corpus(txt)
    expect_equal(
        textstat_collocations(txt, min_count = 2, size = 3),
        textstat_collocations(corp, min_count = 2, size = 3)
    )
    expect_equal(
        textstat_collocations(tokens(txt), min_count = 2, size = 3),
        textstat_collocations(tokens(txt, hash = FALSE), min_count = 2, size = 3)
    )
    
    ## THIS SHOULD BE THE SAME, BUT IS NOT BECAUSE THE REMOVED PUNCTUATION BECOMES
    ## PADS, AND IS COUNTED, WHEN IT SHOULD NOT BE COUNTED AT ALL
    toks <- tokens(txt)
    seqs_corp <- textstat_collocations(corp, method = "lambda", min_count = 2, size = 3)
    seqs_toks <- textstat_collocations(tokens_remove(toks, "\\p{P}", valuetype = "regex", padding = TRUE), method = "lambda", min_count = 2, size = 3)
    expect_equal(
        seqs_corp[, 1:3],
        seqs_toks[match(seqs_corp$collocation, seqs_toks$collocation), 1:3],
        check.attributes = FALSE
    )
})
