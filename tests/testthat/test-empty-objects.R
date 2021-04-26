test_that("empty corpus works", {
  
  corp1 <- corpus(character(), docvars = data.frame(var1 = integer()))
  expect_equal(ndoc(corp1), 0)
  expect_equal(docvars(corp1),
               data.frame(var1 = integer()))
  expect_output(print(corp1),
                "Corpus consisting of 0 documents and 1 docvar.")
  
  corp2 <- corpus(data.frame(text = character(), var2 = numeric()))
  expect_equal(ndoc(corp2), 0)
  expect_equal(docvars(corp2),
               data.frame(var2 = numeric()))
  expect_output(print(corp2),
                "Corpus consisting of 0 documents and 1 docvar.")
  
  corp3 <- c(corp1, corp2)
  expect_equal(ndoc(corp3), 0)
  expect_equal(docvars(corp3),
               data.frame(var1 = integer(), var2 = numeric()))
  expect_output(print(corp3),
                "Corpus consisting of 0 documents and 2 docvars.")
  
  # works with corpus methods
  expect_silent(corpus(corp3))
  expect_silent(corpus_subset(corp3))
  expect_silent(corpus_segment(corp3))
  expect_silent(corpus_reshape(corp3))
  expect_silent(corpus_group(corp3))
})

test_that("empty tokens works", {
  
  corp1 <- corpus(character(), docvars = data.frame(var1 = integer()))
  corp2 <- corpus(data.frame(text = character(), var2 = numeric()))
  
  toks1 <- tokens(corp1)
  expect_output(print(toks1),
                "Tokens consisting of 0 documents and 1 docvar.")
  
  toks2 <- tokens(corp2)
  expect_output(print(toks2),
                "Tokens consisting of 0 documents and 1 docvar.")
  
  toks3 <- c(toks1, toks2)
  expect_equal(ndoc(toks3), 0)
  expect_equal(docvars(toks3),
               data.frame(var1 = integer(), var2 = numeric()))
  expect_output(print(toks3),
                "Tokens consisting of 0 documents and 2 docvars.")
  
  # works with tokens methods
  expect_silent(tokens(toks3))
  expect_silent(tokens_subset(toks3))
  expect_silent(tokens_select(toks3, stopwords("en")))
  expect_silent(tokens_segment(toks3, pattern = "."))
  expect_silent(tokens_group(toks3))
  expect_silent(as.tokens(list()))

})

test_that("empty DFM works", {

    toks1 <- tokens(corpus(character(), docvars = data.frame(var1 = integer())))
    toks2 <- tokens(corpus(data.frame(text = character(), var2 = numeric())))
    
    dfmat1 <- dfm(toks1)
    expect_output(print(dfmat1),
                  "Document-feature matrix of: 0 documents, 0 features (0.00% sparse) and 1 docvar.", 
                  fixed = TRUE)
    
    dfmat2 <- dfm(toks2)
    expect_output(print(dfmat2),
                  "Document-feature matrix of: 0 documents, 0 features (0.00% sparse) and 1 docvar.", 
                  fixed = TRUE)
    
    dfmat3 <- rbind(dfmat1, dfmat2)
    expect_equal(ndoc(dfmat3), 0)
    # fixed #2111
    # expect_equal(docvars(dfmat3),
    #              data.frame(var1 = integer(), var2 = numeric()))
    # expect_output(print(dfmat3),
    #               "Document-feature matrix of: 0 documents, 0 features (0.00% sparse) and 2 docvar.", 
    #               fixed = TRUE)
    
    # works with dfm methods
    expect_silent(dfm(dfmat3))
    expect_silent(dfm_subset(dfmat3))
    expect_silent(dfm_select(dfmat3, stopwords("en")))
    expect_silent(dfm_group(dfmat3))
    expect_silent(as.dfm(matrix(nrow = 0, ncol = 0)))
    expect_silent(as.dfm(Matrix::Matrix(nrow = 0, ncol = 0)))

})
