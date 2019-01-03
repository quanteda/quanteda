context("test textstat_entropy")

test_that("test textstat_entropy is working", {
    mt <- data_dfm_lbgexample
    expect_equal(apply(mt, 1, entropy::entropy, unit = "log2"),
                 textstat_entropy(mt, "documents"))
    
    expect_equal(apply(mt, 2, entropy::entropy, unit = "log2"),
                 textstat_entropy(mt, "features"))
    
    expect_equal(apply(mt, 1, entropy::entropy, unit = "log10"),
                 textstat_entropy(mt, "documents", base = 10))
    
    expect_equal(apply(mt, 2, entropy::entropy, unit = "log10"),
                 textstat_entropy(mt, "features", base = 10))

})
