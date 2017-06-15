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
    
    seqs <- sequences(toks, min_count = 1, size=3:4)
    expect_equal(seqs$collocation, c('E G G', 'E G G G', 'E E G G', 'G G G', 'E E G'))
})

test_that("test that argument 'method'", {
    
    toks <- tokens('c o c g o c w o g c')
    
    #the largest count is: counts(NOTcNOTgNOTo)=4; and it has a positive sign in "unigram"
    seqs <- sequences(toks, min_count = 1, method = "unigram", size = 3)
    expect_equal(seqs$collocation[3], 'c g o')
    expect_gt(seqs$lambda[3], 0)              
    
    #the largest count is: counts(NOTcNOTgNOTo)=4; and it has a negtive sign in "all_subtuples"
    seqs <- sequences(toks, min_count = 1, method = "all_subtuples", size = 3)
    expect_equal(seqs$collocation[5], 'c g o')
    expect_lt(seqs$lambda[5], 0)
    
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
     
     #dice
     expect_equal(seqs$dice[1], 0.625)
     
     #pmi
     expect_equal(seqs$pmi[1], log(2.5*(9+0.5*8)/(2.5+1.5)/(2.5+1.5)))
 })

test_that("test the correctness of significant", {
    toks <- tokens('capital other capital gains other capital word2 other gains capital')
    seqs <- sequences(toks, min_count=1, size = 2, smoothing = 0)

    #dice
    expect_equal(seqs$dice[1], 0.667, tolerance = 1e-3)
    
    #pmi
    expect_equal(seqs$pmi[1], log(2*9/(2+1)/(2+1)))
    
    #chi2
    expect_equal(seqs$chi2[1], 2.25)
    
    #log likelihood ratio
    expect_equal(seqs$logratio[1], 2.231, tolerance = 1e-3)
})

