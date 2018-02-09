context("test textmodel_wordscores")

test_that("test wordscores on LBG data", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, newdata = data_dfm_lbgexample[6, ], interval = "none")
    expect_equal(unclass(pr), c(V1 = -.45), tolerance = .01)
    
    pr2 <- predict(ws, interval = "none")
    expect_is(pr2, "numeric")
    expect_equal(names(pr2), docnames(data_dfm_lbgexample))
    expect_equal(pr2["V1"], c(V1 = -.45), tolerance = .01)
    
    pr3 <- predict(ws, se.fit = TRUE, interval = "none")
    expect_is(pr3, "list")
    expect_equal(names(pr3), c("fit", "se.fit"))
    expect_equal(pr3$se.fit[6], 0.01, tolerance = .01)
})

test_that("a warning occurs for mv with multiple ref scores", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    expect_warning(predict(ws, rescaling = "mv"),
                   "More than two reference scores found with MV rescaling; using only min, max values")
})

test_that("test wordscores on LBG data, MV rescaling", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- suppressWarnings(predict(ws, rescaling = "mv", interval = "none"))
    expect_equal(pr["V1"], c(V1 = -.51), tolerance = .001)
})

test_that("test wordscores on LBG data, LBG rescaling", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, rescaling = "lbg", interval = "none")
    expect_equal(pr["V1"], c(V1 = -.53), tolerance = .01)
})

test_that("test wordscores fitted and predicted", {
    y <- c(seq(-1.5, 1.5, .75), NA)
    ws <- textmodel_wordscores(data_dfm_lbgexample, y)
    expect_equal(ws$x, data_dfm_lbgexample)
    expect_equal(ws$y, y)
    expect_equal("textmodel_wordscores.dfm", as.character(ws$call)[1])
})

test_that("coef works for wordscores fitted", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    expect_equal(coef(ws), ws$wordscores)
    expect_equal(coef(ws), coefficients(ws))
})

# test_that("coef works for wordscores predicted, rescaling = none", {
#     ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
#     pr <- predict(ws, rescaling = "none")
#     expect_equal(coef(pr)$coef_feature, ws@Sw)
#     expect_true(is.null(coef(ws)$coef_feature_se))
#     expect_equal(coef(pr)$coef_document, pr@textscores$textscore_raw)
#     expect_equal(coef(pr)$coef_document_se, pr@textscores$textscore_raw_se)
# })

# test_that("coef works for wordscores predicted, rescaling = mv", {
#     pr <- suppressWarnings(
#         predict(textmodel_wordscores(data_dfm_lbgexample, 
#                                      c(seq(-1.5, 1.5, .75), NA)), 
#                 rescaling = "mv")
#     )
#     expect_equal(coef(pr)$coef_document, pr@textscores$textscore_mv)
#     expect_equal(
#         coef(pr)$coef_document_se, 
#         (pr@textscores$textscore_mv - pr@textscores$textscore_mv_lo) / 1.96,
#         tolerance = .001
#     )
# })

# test_that("coef works for wordscores predicted, rescaling = lbg", {
#     pr <- predict(textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA)), 
#                   rescaling = "lbg")
#     expect_equal(coef(pr)$coef_document, pr@textscores$textscore_lbg)
#     expect_equal(
#         coef(pr)$coef_document_se, 
#         (pr@textscores$textscore_lbg - pr@textscores$textscore_lbg_lo) / 1.96,
#         tolerance = .001
#     )
# })



test_that("coef and coefficients are the same", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, interval = "none")
    expect_equal(coef(ws), coefficients(ws))
    # expect_equal(coef(pr), coefficients(pr))
})

test_that("confidence intervals all work", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    
    pr <- predict(ws, se.fit = TRUE, interval = "confidence", rescaling = "none")
    expect_equal(names(pr), c("fit", "se.fit"))
    expect_equal(colnames(pr$fit), c("fit", "lwr", "upr"))
    expect_is(pr$fit, "matrix")
    
    pr_lbg <- predict(ws, se.fit = TRUE, interval = "confidence", rescaling = "lbg")
    expect_equal(names(pr_lbg), c("fit", "se.fit"))
    expect_equal(colnames(pr_lbg$fit), c("fit", "lwr", "upr"))
    expect_is(pr_lbg$fit, "matrix")
    
    pr_mv <- suppressWarnings(
        predict(ws, se.fit = TRUE, interval = "confidence", rescaling = "mv")
    )
    expect_equal(names(pr_mv), c("fit", "se.fit"))
    expect_equal(colnames(pr_mv$fit), c("fit", "lwr", "upr"))
    expect_is(pr_mv$fit, "matrix")
    expect_equal(pr_mv$fit[c(1, 5), "fit"], c(R1 = -1.5, R5 = 1.5))
})

test_that("textmodel_wordscores print methods work", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
    expect_output(
        quanteda:::print.textmodel_wordscores(ws),
        "^\\nCall:\\ntextmodel_wordscores\\.dfm\\(.*Scale: linear;.*37 scored features\\.$"
    )
    
    sws <- summary(ws)
    expect_output(
        quanteda:::print.summary.textmodel(sws),
        "^\\nCall:\\ntextmodel_wordscores\\.dfm\\(.*Reference Document Statistics:.*Wordscores:\\n"
    )
})

test_that("additional quanteda methods", {
    ws <- textmodel_wordscores(data_dfm_lbgexample, c(-1.5, NA, NA, NA, .75, NA))
    expect_equal(ndoc(ws), 6)
    expect_equal(nfeat(ws), 37)
    expect_equal(docnames(ws), docnames(data_dfm_lbgexample))
    expect_equal(featnames(ws), featnames(data_dfm_lbgexample))
})
