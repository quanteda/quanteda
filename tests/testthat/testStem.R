library(quanteda)

test_that("simple wordstem test to test testing.", {
    expect_equal(wordstem('testing'), 'test')
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
    wordstem(mydfm)
    expect_equal(nfeature(wordstem(mydfm)), 5)
    
})