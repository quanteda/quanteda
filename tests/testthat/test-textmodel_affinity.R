context("tests of textmodel_affinity")

test_that("textmodel_affinity works as expected",  {
    fitted <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
    predicted <- predict(fitted)
    expect_gte(coef(predicted)["V1", "L"], 0.95)
    expect_equal(rownames(coef(predicted)), c("R1","R2","R3","R4","R5","V1"))
})

test_that("textmodel_affinity works for tolower = TRUE dfm objects (#1338)", {
    txt <- c("A", "B", "C", "a")
    dfm1 <- dfm(txt, tolower = TRUE)
    dfm2 <- dfm(txt, tolower = FALSE)

    expect_output(
        print(textmodel_affinity(dfm1, y = c("one", "two", NA, NA))),
        "Training documents per class:one: 1, two: 1; total training features: 3"
    )
    expect_output(
        print(textmodel_affinity(dfm2, y = c("one", "two", NA, NA))),
        "Training documents per class:one: 1, two: 1; total training features: 4"
    )
})

test_that("raises error when dfm is empty (#1419)",  {
    
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(textmodel_affinity(mx, y = c(-1, NA, NA, NA, 1, NA)),
                 quanteda:::message_error("dfm_empty"))

})
