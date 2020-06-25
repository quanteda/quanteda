context("test textstat_entropy")

test_that("test textstat_entropy is working", {
    skip_if_not_installed("entropy")
    
    mt <- data_dfm_lbgexample
    expect_equal(unname(apply(mt, 1, entropy::entropy, unit = "log2")),
                 textstat_entropy(mt, "documents")[["entropy"]])
    expect_equal(unname(apply(mt, 2, entropy::entropy, unit = "log2")),
                 textstat_entropy(mt, "features")[["entropy"]])
    expect_equal(unname(apply(mt, 1, entropy::entropy, unit = "log10")),
                 textstat_entropy(mt, "documents", base = 10)[["entropy"]])
    expect_equal(unname(apply(mt, 2, entropy::entropy, unit = "log10")),
                 textstat_entropy(mt, "features", base = 10)[["entropy"]])
    expect_is(textstat_entropy(mt, "documents"), "data.frame")
})

test_that("test textstat_entropy works with empty documents or features", {
    mt <- data_dfm_lbgexample
    mt[3,] <- 0
    mt[5,] <- 0
    mt <- as.dfm(mt)
    expect_silent(textstat_entropy(mt, "documents"))
    expect_silent(textstat_entropy(mt, "features"))
})
