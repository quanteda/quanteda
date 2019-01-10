context("test dfm_conform")

test_that("test dfm_conform", {
    
    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    mt <- dfm(txt, tolower = FALSE)

    mt_conf1 <- dfm_conform(mt, c("aa", "zz", "xx", "bb"))
    expect_identical(
        featnames(mt_conf1),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(mt_conf1),
        c("doc1", "doc2")
    )
    expect_identical(
        colSums(mt_conf1),
        c("aa" = 2, "zz" = 0, "xx" = 0, "bb" = 2)
    )
    
    mt_conf2 <- dfm_conform(mt, dfm("aa zz xx bb"))
    expect_identical(
        featnames(mt_conf2),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(mt_conf2),
        c("doc1", "doc2")
    )
    expect_identical(
        colSums(mt_conf2),
        c("aa" = 2, "zz" = 0, "xx" = 0, "bb" = 2)
    )
    
    expect_error(dfm_conform(mt, c(TRUE, FALSE)),
                 "Features must be a character vector or dfm")
    expect_error(dfm_conform(mt, 1:3),
                 "Features must be a character vector or dfm")
    
})
