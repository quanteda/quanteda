context("test dfm_match")

test_that("dfm_match works", {

    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    mt <- dfm(txt, tolower = FALSE)

    mt_conf1 <- dfm_match(mt, c("aa", "zz", "xx", "bb"))
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

    mt_conf2 <- dfm_match(mt, featnames(dfm("aa zz xx bb")))
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
    
})

test_that("dfm_match works with padding", {
    toks <- tokens("aa bb !", padding = TRUE, remove_punct = TRUE)
    dfmt <- dfm(toks)
    expect_identical(
        featnames(dfm_match(dfmt, c("aa", "bb", "cc", ""))),
        c("aa", "bb", "cc", "")
    )
})

test_that("dfm_match coerce non-character feature", {
    
    txt <- c(doc1 = "TRUE TRUE FALSE",
             doc2 = "1 2 100")
    dfmat <- dfm(txt, tolower = FALSE)
    expect_equal(featnames(dfm_match(dfmat, c(TRUE, FALSE))),
                 c("TRUE", "FALSE"))
    expect_equal(featnames(dfm_match(dfmat, c(100, 1))),
                 c("100", 1))

})

