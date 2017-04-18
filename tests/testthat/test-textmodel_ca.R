context('Testing textmodel-ca.R')

ie2010dfm <- dfm(data_corpus_irishbudget2010)

test_that("textmodel-ca (rsvd) works as expected as ca::ca", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm)
    expect_equal(wca$rowdist, wtca$rowdist, tolerance = 1e-6)
    expect_equal(wca$coldist, wtca$coldist, tolerance = 1e-6)
    
    expect_equal(abs(wca$rowcoord[,1]), abs(wtca$rowcoord[,1]), tolerance = 1e-6)
    expect_equal(abs(wca$colcoord[,1]), abs(wtca$colcoord[,1]), tolerance = 1e-6)
    
    expect_equal(abs(wca$rowcoord[,2]), abs(wtca$rowcoord[,2]), tolerance = 1e-6)
    expect_equal(abs(wca$colcoord[,2]), abs(wtca$colcoord[,2]), tolerance = 1e-6)
    
    expect_equal(wca$rowinertia, wtca$rowinertia, tolerance = 1e-6)
    expect_equal(wca$colinertia, wtca$colinertia, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca works as expected as ca::ca : use mt", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm, threads = 2, sparse = TRUE)
    
    expect_gt(cor(wca$rowdist, wtca$rowdist), 0.99)
    expect_gt(cor(wca$coldist, wtca$coldist), 0.99)
    
    expect_gt(cor(abs(wca$rowcoord[,1]), abs(wtca$rowcoord[,1])), 0.99)
    expect_gt(cor(abs(wca$colcoord[,1]), abs(wtca$colcoord[,1])), 0.99)
    
    expect_gt(cor(abs(wca$rowcoord[,2]), abs(wtca$rowcoord[,2])), 0.99)
    expect_gt(cor(abs(wca$colcoord[,2]), abs(wtca$colcoord[,2])), 0.99)
    
    expect_gt(cor(wca$rowinertia, wtca$rowinertia), 0.99)
    expect_gt(cor(wca$colinertia, wtca$colinertia), 0.99)
    
    cc <- cor(wca$sv[1:length(wtca$sv)], wtca$sv)
    expect_gt(cc, 0.99)
})

test_that("textmodel-ca works as expected as ca::ca: for given number of dimension", {
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(ie2010dfm))
    wtca <- textmodel_ca(ie2010dfm, nd = 10)
    expect_equal(wca$rowdist, wtca$rowdist, tolerance = 1e-6)
    expect_equal(wca$coldist, wtca$coldist, tolerance = 1e-6)
    
    expect_equal(abs(wca$rowcoord[,1]), abs(wtca$rowcoord[,1]), tolerance = 1e-6)
    expect_equal(abs(wca$colcoord[,1]), abs(wtca$colcoord[,1]), tolerance = 1e-6)
    
    expect_equal(abs(wca$rowcoord[,2]), abs(wtca$rowcoord[,2]), tolerance = 1e-6)
    expect_equal(abs(wca$colcoord[,2]), abs(wtca$colcoord[,2]), tolerance = 1e-6)
    
    expect_equal(wca$rowinertia, wtca$rowinertia, tolerance = 1e-6)
    expect_equal(wca$colinertia, wtca$colinertia, tolerance = 1e-6)
    
    expect_equal(wca$sv[1:length(wtca$sv)], wtca$sv, tolerance = 1e-6)
})

test_that("textmodel-ca(sparse) works as expected on another dataset", {
    usdfm <- dfm(data_corpus_inaugural, verbose = FALSE)
    skip_if_not_installed("ca")
    wca <- ca::ca(as.matrix(usdfm))
    wtca <- textmodel_ca(usdfm, threads = 2, sparse = TRUE)
    
    expect_gt(cor(wca$rowdist, wtca$rowdist), 0.99)
    expect_gt(cor(wca$coldist, wtca$coldist), 0.99)
    
    expect_gt(cor(abs(wca$rowcoord[,1]), abs(wtca$rowcoord[,1])), 0.99)
    expect_gt(cor(abs(wca$colcoord[,1]), abs(wtca$colcoord[,1])), 0.99)
    
    expect_gt(cor(abs(wca$rowcoord[,2]), abs(wtca$rowcoord[,2])), 0.99)
    expect_gt(cor(abs(wca$colcoord[,2]), abs(wtca$colcoord[,2])), 0.99)
    
    expect_gt(cor(wca$rowinertia, wtca$rowinertia), 0.99)
    expect_gt(cor(wca$colinertia, wtca$colinertia), 0.99)
    
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

test_that("ca coefficients methods work", {
    camodel <- textmodel_ca(data_dfm_LBGexample)
    expect_equal(coef(camodel), coefficients(camodel))
    expect_equal(
        coef(camodel, doc_dim = 2)$coef_document,
        camodel$rowcoord[, 2]
    )
})

test_that("ca textplot_scale1d method works", {
    camodel <- textmodel_ca(data_dfm_LBGexample)
    textplot_scale1d(camodel, margin = "document")
    expect_error(
        textplot_scale1d(camodel, margin = "features"),
        "textplot_scale1d for features not implemented for CA models"
    )
})

test_that("ca sparse = FALSE with threads > 1 issues warning (#663)", {
    expect_warning(
        textmodel_ca(data_dfm_LBGexample, sparse = FALSE, threads = 2),
        "threads reset to 1 when sparse = FALSE"
    )
})

