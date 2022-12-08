test_that("check if objects are S4", {
  
  mat1 <- matrix(0)
  class(mat1) <- "dfm"
  expect_false(is.dfm(mat1))
  
  mat2 <- matrix(0)
  class(mat2) <- "fcm"
  expect_false(is.fcm(mat2))
  
  ls <- list()
  class(ls) <- "dictionary2"
  expect_false(is.dictionary(ls))
  
})

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
    dfmt <- dfm(tokens(data_corpus_inaugural[1:2]))

    expect_true(is.dfm(data_dfm_pre2))
    expect_true(is.dfm(dfmt))

    expect_true(quanteda:::is_pre2(data_dfm_pre2))
    expect_false(quanteda:::is_pre2(as.dfm(data_dfm_pre2)))
    expect_false(quanteda:::is_pre2(dfmt))
})

test_that("fcm is/as methods work with old and new formats", {
    load("../data/pre_v2_objects/data_fcm_pre2.rda")
    fcmt <- fcm(dfm(tokens(data_corpus_inaugural[1:2])))
    
    expect_true(is.fcm(data_fcm_pre2))
    expect_true(is.fcm(fcmt))
    
    expect_true(quanteda:::is_pre2(data_fcm_pre2))
    expect_false(quanteda:::is_pre2(as.fcm(data_fcm_pre2)))
    expect_false(quanteda:::is_pre2(fcmt))
})

