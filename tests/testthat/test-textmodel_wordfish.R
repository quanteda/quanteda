context('Testing textmodel-wordfish.R')

ie2010dfm <- dfm(data_corpus_irishbudget2010)

test_that("textmodel-wordfish (sparse) works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
    
    # dense methods
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE)
    cc<-cor(wfm_d@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})

test_that("textmodel-wordfish works as expected: dense vs sparse vs sparse+mt", {
    #ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE)

    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    cc<-cor(wfm_d@theta, wfm_mt@theta)
    expect_gt(cc, 0.99)
})

test_that("print/show/summary method works as expected", {
    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    expect_output(print(wfm_mt), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(print(wfm_mt), "^Fitted wordfish model:")
    expect_output(print(wfm_mt), "Estimated feature scores:")
    
    # show method
    expect_output(show(wfm_mt), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(show(wfm_mt), "^Fitted wordfish model:")
    expect_output(show(wfm_mt), "Estimated feature scores:")
    
    # summary method
    expect_output(summary(wfm_mt), "[ ]*theta[ ]*SE[ ]*lower")
    expect_output(summary(wfm_mt), "Call:\n\ttextmodel_wordfish\\(x = ie2010dfm, dir = c\\(6, 5\\), sparse = TRUE\\)")
    expect_output(summary(wfm_mt), "Estimated document positions:")
    
    # sparse but not mt
    expect_output(print(wfm_s), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(print(wfm_s), "^Fitted wordfish model:")
    expect_output(print(wfm_s), "Estimated feature scores:")
    
    # show method
    expect_output(show(wfm_s), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(show(wfm_s), "^Fitted wordfish model:")
    expect_output(show(wfm_s), "Estimated feature scores:")
    
    # summary method
    expect_output(summary(wfm_s), "[ ]*theta[ ]*SE[ ]*lower")
    expect_output(summary(wfm_s), "Call:\n\ttextmodel_wordfish\\(x = ie2010dfm, dir = c\\(6, 5\\), sparse = TRUE\\)")
    expect_output(summary(wfm_s), "Estimated document positions:")
})

test_that("coef works for wordfish fitted", {
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = TRUE)
    expect_equal(coef(wfm_mt)$coef_feature, wfm_mt@beta, tolerance = 1e-8)
    expect_true(is.null(coef(wfm_mt)$coef_feature_se))
    expect_equal(coef(wfm_mt)$coef_document, wfm_mt@theta, tolerance = 1e-8)
    expect_equal(coef(wfm_mt)$coef_document_se, wfm_mt@se.theta, tolerance = 1e-8)
    expect_equal(coef(wfm_mt)$coef_document_offset, wfm_mt@alpha, tolerance = 1e-8)
    expect_equal(coef(wfm_mt)$coef_feature_offset, wfm_mt@psi, tolerance = 1e-8)

    # "for wordfish, coef and coefficients are the same", {
    expect_equal(coef(wfm_mt), coefficients(wfm_mt), tolerance = 1e-8)
    expect_equal(coef(wfm_mt), coefficients(wfm_mt), tolerance = 1e-8)
})

test_that("textmodel-wordfish works for quasipoisson - feature as expected: dense vs sparse vs sparse+mt", {
    #ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE,
                                dispersion = "quasipoisson", dispersion_floor = 0)
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                 dispersion = "quasipoisson", dispersion_floor = 0)
    cc<-cor(wfm_d@theta, wfm_mt@theta)
    expect_gt(cc, 0.99)
    
    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                dispersion = "quasipoisson", dispersion_floor = 0)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
})

test_that("textmodel-wordfish works for quasipoisson - overall as expected: dense vs sparse vs sparse+mt", {
    #ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE,
                                dispersion = "quasipoisson", dispersion_level = "overall")
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                 dispersion = "quasipoisson", dispersion_level = "overall")
    cc<-cor(wfm_d@theta, wfm_mt@theta)
    expect_gt(cc, 0.99)
    
    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                dispersion = "quasipoisson", dispersion_level = "overall")
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
})

test_that("textmodel-wordfish (sparse) works as expected on another dataset", {
    usdfm <- dfm(data_corpus_inaugural, verbose = FALSE)
    options(mt = TRUE)
    wfm_s <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 0.5)
    wfm_d <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = FALSE)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
    # with different sparsity of residual matrix
    wfm_s <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 1)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
    wfm_s <- textmodel_wordfish(usdfm, dir = c(6,5), sparse = TRUE, svd_sparse = TRUE, residual_floor = 2)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
})
# reset the global setting
.onAttach()
