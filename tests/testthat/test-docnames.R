context("test docnames")

test_that("docnames always return names even if there aren't", {
    
    corp <- corpus(c('aaa', 'bbb', 'ccc'))
    expect_equal(length(docnames(corp)), ndoc(corp))
    
    toks <- as.tokens(list('aaa', 'bbb', 'ccc'))
    expect_equal(length(docnames(toks)), ndoc(toks))
    
})

