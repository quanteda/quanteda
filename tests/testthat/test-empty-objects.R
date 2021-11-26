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
  expect_s3_class(corpus(corp3), "corpus")
  expect_s3_class(corpus_subset(corp3), "corpus")
  expect_s3_class(corpus_segment(corp3), "corpus")
  expect_s3_class(corpus_reshape(corp3), "corpus")
  expect_s3_class(corpus_trim(corp3), "corpus")
  expect_s3_class(corpus_group(corp3), "corpus")
  expect_s3_class(corpus_sample(corp3), "corpus")
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
  expect_s3_class(tokens(toks3), "tokens")
  expect_s3_class(tokens_subset(toks3), "tokens")
  expect_s3_class(tokens_select(toks3, stopwords("en")), "tokens")
  expect_s3_class(tokens_select(toks3, stopwords("en"), padding = TRUE), "tokens")
  expect_s3_class(tokens_lookup(toks3, data_dictionary_LSD2015), "tokens")
  expect_s3_class(tokens_segment(toks3, pattern = "."), "tokens")
  expect_s3_class(tokens_group(toks3), "tokens")
  expect_s3_class(tokens_sample(toks3), "tokens")
  expect_s3_class(tokens_wordstem(toks3), "tokens")
  expect_s3_class(as.tokens(list()), "tokens")
  expect_s3_class(kwic(toks3, stopwords("en")), "kwic")
  expect_s4_class(fcm(toks3), "fcm")
  expect_equal(types(toks3), character())
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
    
    expect_equal(docvars(dfmat3),
                 data.frame(var1 = integer(), var2 = numeric()))
    expect_output(print(dfmat3),
                  "Document-feature matrix of: 0 documents, 0 features (0.00% sparse) and 2 docvars.",
                  fixed = TRUE)

    # works with dfm methods
    expect_s4_class(dfm(dfmat3), "dfm")
    expect_s4_class(dfm_subset(dfmat3), "dfm")
    expect_s4_class(dfm_select(dfmat3, stopwords("en")), "dfm")
    expect_s4_class(dfm_select(dfmat3, stopwords("en"), padding = TRUE), "dfm")
    expect_s4_class(dfm_lookup(dfmat3, data_dictionary_LSD2015), "dfm")
    expect_s4_class(dfm_group(dfmat3), "dfm")
    expect_s4_class(dfm_sample(dfmat3), "dfm")
    expect_s4_class(dfm_wordstem(dfmat3), "dfm")
    expect_s4_class(dfm_sort(dfmat3), "dfm")
    expect_s4_class(dfm_weight(dfmat3), "dfm")
    expect_s4_class(as.dfm(matrix(nrow = 0, ncol = 0)), "dfm")
    expect_s4_class(as.dfm(Matrix::Matrix(nrow = 0, ncol = 0)), "dfm")
    expect_s4_class(fcm(dfmat3), "fcm")
    expect_equal(featnames(dfmat3), character())
    expect_equal(featfreq(dfmat3), structure(numeric(), names = character()))
})

test_that("subsetting zero documents works", {
  corp <- corpus(c("a b c", "d e f"))
  expect_equal(ndoc(corp[0]), 0)
  
  toks <- tokens(corp)
  expect_equal(ndoc(toks[0]), 0)
  
  dfmat <- dfm(toks)
  expect_equal(ndoc(dfmat[0,]), 0)
})
