context("test robustness of new object class handling")

test_that("corpus is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")

    expect_true(is.corpus(data_corpus_pre2))
    expect_true(is.corpus(data_corpus_inaugural))

    expect_true(quanteda:::is_pre2(data_corpus_pre2))
    expect_false(quanteda:::is_pre2(as.corpus(data_corpus_pre2)))
    #expect_false(quanteda:::is_pre2(data_corpus_inaugural))
})

test_that("tokens is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_tokens_pre2.rda")
    toks <- tokens(data_corpus_inaugural[1:2])

    expect_true(is.tokens(data_tokens_pre2))
    expect_true(is.tokens(toks))

    expect_true(quanteda:::is_pre2(data_tokens_pre2))
    expect_false(quanteda:::is_pre2(as.tokens(data_tokens_pre2)))
    expect_false(quanteda:::is_pre2(toks))
})

test_that("dfm is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_dfm_pre2.rda")
    dfmt <- dfm(data_corpus_inaugural[1:2])

    expect_true(is.dfm(data_dfm_pre2))
    expect_true(is.dfm(data_dfm_pre2))

    expect_true(quanteda:::is_pre2(data_dfm_pre2))
    expect_false(quanteda:::is_pre2(as.dfm(data_dfm_pre2)))
    expect_false(quanteda:::is_pre2(dfmt))
})
