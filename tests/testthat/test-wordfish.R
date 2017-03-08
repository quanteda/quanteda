require(quanteda)
require(testthat)
context('Testing textmodel-wordfish.R')

# load the austin library
# library(austin)
test_that("textmodel-wordfish works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5), version = "parallel")
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc <- cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})
     
test_that("textmodel-wordfish Parallel version works as expected as serial version: poission", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), version = "serial")

    wfm_p <- textmodel_wordfish(ie2010dfm, dir = c(6,5), version = "parallel")
    cc <- cor(wfm_s@theta, wfm_p@theta)
    expect_gt(cc, 0.99)
})

test_that("textmodel-wordfish Parallel version works as expected as serial version: poisson-feature", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), dispersion = "quasipoisson",dispersion_level = "feature")

    wfm_p <- textmodel_wordfish(ie2010dfm, dir = c(6,5), dispersion = "quasipoisson",dispersion_level = "feature",version = "parallel", svd_sparse = T, residual_floor = 0.5)
    cc <- cor(wfm_s@theta, wfm_p@theta)
    expect_gt(cc, 0.99)

})

test_that("textmodel-wordfish Parallel version works as expected as serial version: quasipoisson-overall", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), dispersion = "quasipoisson",dispersion_level = "overall")

    wfm_p <- textmodel_wordfish(ie2010dfm, dir = c(6,5), dispersion = "quasipoisson",dispersion_level = "overall",version = "parallel", svd_sparse = T, residual_floor = 0.5)
    cc <- cor(wfm_s@theta, wfm_p@theta)
    expect_gt(cc, 0.99)

})
