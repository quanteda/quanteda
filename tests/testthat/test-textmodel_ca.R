context('Testing textmodel-ca_sparse.R')

ie2010dfm <- dfm(data_corpus_irishbudget2010)

test_that("textmodel-ca (sparse) works as expected as ca::ca", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca_sparse(ie2010dfm)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca (sparse) works as expected as ca::ca: for given number of dimension", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca_sparse(ie2010dfm, nd = 10)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})
