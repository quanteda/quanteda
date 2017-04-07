context('Testing textmodel-ca.R')

ie2010dfm <- dfm(data_corpus_irishbudget2010)

test_that("textmodel-ca (rsvd) works as expected as ca::ca", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca (RSpectra) works as expected as ca::ca", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm, method = "RSpectra")
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca works as expected as ca::ca : use mt", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm, mt = TRUE)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    cc <- cor(wca$sv[1:length(wtca$sv)], wtca$sv)
    expect_gt(cc, 0.99)
})

test_that("textmodel-ca works as expected as ca::ca: for given number of dimension", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm, nd = 10)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca(sparse) works as expected on another dataset", {
    usdfm <- dfm(data_corpus_inaugural, verbose = FALSE)
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(usdfm))
    wtca <- textmodel_ca(usdfm, mt = TRUE)
    expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
    expect_equal(wca$colmass, wtca$colmass, tolerance = 1e-6)
    
    cc <- cor(wca$sv[1:length(wtca$sv)], wtca$sv)
    expect_gt(cc, 0.99)
})
# test_that("textmodel-ca generates a ca::ca: object, which can call other functions from ca package", {
#     skip_if_not_installed("ca")
#     wca <- ca::ca(smoke)
#     wtca <- textmodel_ca(as.dfm(smoke), nd=5)
#     expect_equal(wca$rowmass, wtca$rowmass, tolerance = 1e-6)
#     
#     # plot method
#     #p <- ca::plot.ca(wtca)
#     #expect_equal(dimnames(p$rows)[[2]][1], "Dim1")
#     
#     # summary method
#     s <- summary(wtca)
#     expect_equal(attr(s, "class"), "summary.ca")
# })
