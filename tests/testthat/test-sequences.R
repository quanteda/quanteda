context('test sequences.R')

toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)

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
    seqs <- sequences(toks_capital, min_count = 1, size = 2:4)
    
    # seqs have the same types
    expect_equivalent(as.list(tokens_compound(toks, seqs, join = FALSE)),
                      list(c("E_E", "a", "b", "c", "E_E_G_G", "E_E_G","E_E", "E_G_G","E_G", "G_G", "f", "E_E", "f", "f", "G_G")))
})

test_that("[ function",{
    toks <- tokens('E E G F a b c E E G G f E E f f G G')
    toks_capital <- tokens_select(toks, "^[A-Z]$", valuetype="regex", 
                                  case_insensitive = FALSE, padding = TRUE)
    seqs <- sequences(toks_capital, min_count = 1)
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

