context("test textmodel_wordscores")

test_that("test wordscores on LBG data", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, newdata = data_dfm_LBGexample[6, ])
    expect_equal(round(pr@textscores[1,1], 2), -.45)
})

test_that("test wordscores on LBG data, MV rescaling", {
    ws <- textmodel_wordscores(data_dfm_LBGexample, c(seq(-1.5, 1.5, .75), NA))
    pr <- predict(ws, rescaling = "mv")
    expect_equal(round(as.numeric(pr@textscores["V1", "textscore_mv"]), 2), -.51)
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
