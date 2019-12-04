context("test robustness of new object class handling")

test_that("corpus is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    
    expect_true(is.corpus(data_corpus_pre2))
    expect_true(is.corpus(data_corpus_inaugural))
    
    expect_true(quanteda:::is_pre2(data_corpus_pre2))
    expect_false(quanteda:::is_pre2(as.corpus(data_corpus_pre2)))
    expect_false(quanteda:::is_pre2(data_corpus_inaugural))
})

test_that("tokens is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_tokens_pre2.rda")
    data_tokens_inaugural <- tokens(data_corpus_inaugural[1:2])
    
    expect_true(is.tokens(data_tokens_pre2))
    expect_true(is.tokens(data_tokens_inaugural))
    
    expect_true(quanteda:::is_pre2(data_tokens_pre2))
    # breaks because as.tokens.tokens() does not add meta attribute
    expect_false(quanteda:::is_pre2(as.tokens(data_tokens_pre2)))
    expect_false(quanteda:::is_pre2(data_tokens_inaugural))
})

test_that("dfm is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_dfm_pre2.rda")
    
    # produces this warning:
    ## Warning message:
    ## Not a validObject(): no slot of name "meta" for this object of class "dfm" 
    data_dfm_inauguralsub <- data_corpus_inaugural[1:2]
    
    expect_true(is.dfm(data_dfm_pre2))
    
    # errors
    expect_true(is.dfm(data_corpus_inaugural[1:2]))
    
    expect_true(is.dfm(dfm(data_corpus_inaugural)))
    expect_true(is.dfm(dfm(c("one two", "three"))))
    
    expect_true(quanteda:::is_pre2(data_dfm_pre2))
    expect_false(quanteda:::is_pre2(as.dfm(data_dfm_pre2)))
    expect_false(quanteda:::is_pre2(data_dfm_lbgexample))
})
