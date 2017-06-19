context('test sequences.R')

toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)

test_that("test that nested argument is working", {
    
    toks <- tokens('E E G F a b c E E G G f E E f f G G')
    toks <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                          case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks, min_count = 1, size= 2, nested = FALSE)
    expect_equal(seqs$collocation, c('E E', 'G G', 'G F'))
    expect_equal(seqs$count, c(3, 2, 1))
    
    seqs_nested <- sequences(toks, min_count = 1, size = 2, nested = TRUE)
    expect_equal(seqs_nested$collocation, c('E E', 'G F', 'G G', 'E G'))
    expect_equal(seqs_nested$count, c(3, 1, 2, 2))
})

test_that("test that argument 'size'", {
    
    toks <- tokens('E E a b c E E G G G f E E f f G G')
    toks <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                          case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks, min_count = 1)
    expect_equal(seqs$collocation, c('G G', 'E E', 'E G'))
    
    seqs <- sequences(toks, min_count = 1, size=3:4, method = "lambda1")
    expect_equal(seqs$collocation, c('E G G', 'E G G G', 'E E G G', 'G G G', 'E E G'))
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

test_that("test that sequences works with tokens_compound", {
    
    toks <- tokens('E E a b c E E G G f E E f f G G')
    toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                          case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks_capital, min_count = 1, nested = FALSE, size = 2:4)
    
    # seqs have the same types
    expect_equivalent(as.list(tokens_compound(toks, seqs, join = FALSE)),
                      list(c("E_E", "a", "b", "c", "E_E_G_G", "E_E_G","E_E", "G_G", "f", "E_E", "f", "f", "G_G")))
})

test_that("[ function",{
    toks <- tokens('E E G F a b c E E G G f E E f f G G')
    toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                                  case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks_capital, min_count = 1, nested = TRUE)
    a_seq <- seqs[1]
    
    expect_equal(a_seq$collocation, 'E E')
    expect_equal(class(a_seq), c("sequences", 'data.frame'))
})

test_that("as.tokens.sequences function",{
    toks <- tokens('E E a b c E E G G f E E f f G G')
    toks <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                                  case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks, min_count = 1)
    tokens <- as.tokens(seqs)
    
    expect_equal(length(tokens), 3)
    expect_equal(class(tokens), c("tokens", "tokenizedTexts"))
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
    expect_equal(seqs$logratio[1], 2.231, tolerance = 1e-3)
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
    
    seqs <- sequences(tokens(txt), size = 3, smoothing =0)
    expect_equal(seqs$collocation[3], 'capital gains tax')
    expect_equal(seqs$logratio[3], statss$lrt, tolerance = 1e-3)
    expect_equal(seqs$chi2[3], statss$pearson, tolerance = 1e-3)
})
