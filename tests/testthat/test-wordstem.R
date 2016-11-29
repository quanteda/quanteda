library(quanteda)

test_that("character wordstem test to test testing.", {
    expect_equal(char_wordstem('testing'), 'test')
})


test_that("can wordstem dfms with zero features and zero docs", {
    
    # zero feature documents
    dfm1 <- dfm(c("one", "0"), stem = TRUE, removeNumbers = TRUE)
    dfm2 <- dfm(c("one", "!!"), stem = TRUE, removePunct = TRUE)
    expect_equal(ndoc(dfm1), ndoc(dfm2), 2)
    
    # features with zero docfreq
    mydfm <- dfm(c("stemming porter three", "stemming four five"))
    mydfm[2, 4] <- 0
    mydfm <- new("dfmSparse", mydfm)
    dfm_wordstem(mydfm)
    expect_equal(nfeature(dfm_wordstem(mydfm)), 5)
    
})