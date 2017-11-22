context("tests of textmodel_mixfit")

test_that("textmodel_affinity works as expected",  {
    fitted <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
    predicted <- predict(fitted)
    expect_gte(coef(predicted)["V1", "L"], 0.95)
    expect_equal(rownames(coef(predicted)), c("R1","R2","R3","R4","R5","V1"))
})
