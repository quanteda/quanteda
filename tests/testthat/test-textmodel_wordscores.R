context("test textmodel_wordscores")

test_that("test wordscores on LBG data", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, newdata = data_dfm_LBGexample[6, ])
    expect_equal(round(pr@textscores[1,1], 2), -.45)
})

test_that("a warning occurs for mv with multiple ref scores", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    expect_warning(predict(ws, rescaling = "mv"),
                   "More than two reference scores found with MV rescaling; using only min, max values")
})

test_that("test wordscores on LBG data, MV rescaling", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, rescaling = "mv")
    expect_equal(round(pr@textscores["V1", "textscore_mv"], 2), -.51)
})

test_that("test wordscores on LBG data, LBG rescaling", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, rescaling = "lbg")
    expect_equal(round(as.numeric(pr@textscores["V1", "textscore_lbg"]), 2), -.53)
})

test_that("test wordscores fitted and predicted", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws)
    expect_equal(ws@x, pr@x)
    expect_equal(ws@y, pr@y)
    expect_equal(ws@Sw, pr@Sw)
    expect_equal(ws@call, pr@call)
})

test_that("coef works for wordscores fitted", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    expect_equal(coef(ws)$coef_feature, ws@Sw)
    expect_true(is.null(coef(ws)$coef_feature_se))
    expect_true(is.null(coef(ws)$coef_document))
    expect_true(is.null(coef(ws)$coef_document_se))
})

test_that("coef works for wordscores predicted, rescaling = none", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, rescaling = "none")
    expect_equal(coef(pr)$coef_feature, ws@Sw)
    expect_true(is.null(coef(ws)$coef_feature_se))
    expect_equal(coef(pr)$coef_document, pr@textscores$textscore_raw)
    expect_equal(coef(pr)$coef_document_se, pr@textscores$textscore_raw_se)
})

test_that("coef works for wordscores predicted, rescaling = mv", {
    pr <- predict(textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA)), 
                  rescaling = "mv")
    expect_equal(coef(pr)$coef_document, pr@textscores$textscore_mv)
    expect_equal(coef(pr)$coef_document_se, NULL)
})

test_that("coef works for wordscores predicted, rescaling = lbg", {
    pr <- predict(textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA)), 
                  rescaling = "lbg")
    expect_equal(coef(pr)$coef_document, pr@textscores$textscore_lbg)
    expect_equal(
        coef(pr)$coef_document_se, 
        (pr@textscores$textscore_lbg - pr@textscores$textscore_lbg_lo) / 1.96
    )
})



test_that("coef and coefficients are the same", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws)
    expect_equal(coef(ws), coefficients(ws))
    expect_equal(coef(pr), coefficients(pr))
})
