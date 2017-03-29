context('Testing textmodel-wordfish.R')

ie2010dfm <- dfm(data_corpus_irishbudget2010)

test_that("textmodel-wordfish (sparse) works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})
     
test_that("textmodel-wordfish (dense) works as expected as austin::wordfish", {
    skip_if_not_installed("austin")
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE)
    wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
    cc<-cor(wfm@theta, wfmodelAustin$theta)
    expect_gt(cc, 0.99)
})

test_that("textmodel-wordfish works as expected: dense vs sparse vs sparse+mt", {

    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
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

test_that("textmodel-wordfish (sparse) works as expected on another dataset", {
    usdfm <- dfm(data_corpus_inaugural, verbose = FALSE)
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

test_that("textmodel-wordfish works for quasipoisson - feature as expected: dense vs sparse vs sparse+mt", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE,
                                 dispersion = "quasipoisson", dispersionFloor = 0)
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                dispersion = "quasipoisson", dispersionFloor = 0)
    cc<-cor(wfm_d@theta, wfm_mt@theta)
    expect_gt(cc, 0.99)
    
    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                 dispersion = "quasipoisson", dispersionFloor = 0)
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
})

test_that("textmodel-wordfish works for quasipoisson - overall as expected: dense vs sparse vs sparse+mt", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    wfm_d <- textmodel_wordfish(ie2010dfm, dir = c(6,5), sparse = FALSE,
                                dispersion = "quasipoisson", dispersionLevel = "overall")
    options(mt = TRUE)
    wfm_mt <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                 dispersion = "quasipoisson", dispersionLevel = "overall")
    cc<-cor(wfm_d@theta, wfm_mt@theta)
    expect_gt(cc, 0.99)
    
    options(mt = FALSE)
    wfm_s <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
                                dispersion = "quasipoisson", dispersionLevel = "overall")
    cc<-cor(wfm_d@theta, wfm_s@theta)
    expect_gt(cc, 0.99)
    
})

test_that("print/show/summary method works as expected", {
    ie2010dfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
    
    # sparse + mt
    options(mt = TRUE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    expect_output(print(wfm), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(print(wfm), "^Fitted wordfish model:")
    expect_output(print(wfm), "Estimated feature scores:")
    
    # show method
    expect_output(show(wfm), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(show(wfm), "^Fitted wordfish model:")
    expect_output(show(wfm), "Estimated feature scores:")
    
    # summary method
    expect_output(summary(wfm), "[ ]*theta[ ]*SE[ ]*lower")
    expect_output(summary(wfm), "Call:\n\ttextmodel_wordfish\\(data = ie2010dfm, dir = c\\(6, 5\\)\\)")
    expect_output(summary(wfm), "Estimated document positions:")
    
    # sparse but not mt
    options(mt = FALSE)
    wfm <- textmodel_wordfish(ie2010dfm, dir = c(6,5))
    expect_output(print(wfm), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(print(wfm), "^Fitted wordfish model:")
    expect_output(print(wfm), "Estimated feature scores:")
    
    # show method
    expect_output(show(wfm), "[ ]*Documents[ ]*theta[ ]*SE[ ]*lower")
    expect_output(show(wfm), "^Fitted wordfish model:")
    expect_output(show(wfm), "Estimated feature scores:")
    
    # summary method
    expect_output(summary(wfm), "[ ]*theta[ ]*SE[ ]*lower")
    expect_output(summary(wfm), "Call:\n\ttextmodel_wordfish\\(data = ie2010dfm, dir = c\\(6, 5\\)\\)")
    expect_output(summary(wfm), "Estimated document positions:")
})

test_that("coef works for wordfish fitted", {
    expect_equal(coef(wfm)$coef_feature, wfm@beta)
    expect_true(is.null(coef(wfm)$coef_feature_se))
    expect_equal(coef(wfm)$coef_document, wfm@theta)
    expect_equal(coef(wfm)$coef_document_se, wfm@se.theta)
    expect_equal(coef(wfm)$coef_document_offset, wfm@alpha)
    expect_equal(coef(wfm)$coef_feature_offset, wfm@psi)
})

test_that("for wordfish, coef and coefficients are the same", {
    expect_equal(coef(wfm), coefficients(wfm))
    expect_equal(coef(wfm), coefficients(wfm))
})

# reset the global setting
.onAttach()
