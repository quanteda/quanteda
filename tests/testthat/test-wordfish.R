require(quanteda)
require(testthat)
context('Testing textmodel-wordfish.R')
data_corpus_inaugural
# load the austin library
# library(austin)
test_that("textmodel-wordfish (sparse) works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})
     
test_that("textmodel-wordfish (dense) works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE)
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})

test_that("textmodel-wordfish works as expected: dense vd sparse", {

    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
})

test_that("textmodel-wordfish (sparse) works as expected on another dataset", {
    
    usdfm <- dfm(data_corpus_inaugural, verbose = FALSE)
    wfm_d <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 0.5)
    wfm_s <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = FALSE)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
    # with different sparsity of residual matrix
    wfm_d <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 1)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
    wfm_d <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 2)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
})
