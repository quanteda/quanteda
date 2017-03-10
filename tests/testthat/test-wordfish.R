require(quanteda)
require(testthat)
context('Testing textmodel-wordfish.R')

# load the austin library
# library(austin)
test_that("textmodel-wordfish works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})
     
test_that("textmodel-wordfish works as expected as austin::wordfish", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    wfmodelAustin <- c(1.7798574, -0.5795493, -1.1451491, -0.1929161,  1.8887539, -0.7086047,
                       -0.5035685, -0.6343675, -0.9924275, -0.9817932,  1.2315642,  0.2732467,
                       0.8305827, -0.3419316)
    cc<-cor(wfm@theta, wfmodelAustin)
    expect_gt(cc, 0.99)
})

test_that("print/show/summary method works as expected", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    expect_output(print(wfm), "Documents[ ]*theta[ ]*SE[ ]*lower[ ]*upper")
    expect_output(print(wfm), "^Fitted wordfish model:")
    expect_output(print(wfm), "Estimated feature scores:")
    
    # show method
    expect_output(show(wfm), "Documents[ ]*theta[ ]*SE[ ]*lower[ ]*upper")
    expect_output(show(wfm), "^Fitted wordfish model:")
    expect_output(show(wfm), "Estimated feature scores:")
    
    # summary method
    expect_output(summary(wfm), "theta[ ]*SE[ ]*lower[ ]*upper")
    expect_output(summary(wfm), "Call:\n\ttextmodel_wordfish\\(data = ie2010dfm, dir = c\\(6, 5\\)\\)")
    expect_output(summary(wfm), "Estimated document positions:")
})
