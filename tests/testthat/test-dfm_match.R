test_that("dfm_match works", {
    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    dfmat <- dfm(tokens(txt), tolower = FALSE)

    dfmat_conf1 <- dfm_match(dfmat, c("aa", "zz", "xx", "bb"))
    expect_identical(
        featnames(dfmat_conf1),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(dfmat_conf1),
        c("doc1", "doc2")
    )
    expect_identical(
        colSums(dfmat_conf1),
        c("aa" = 2, "zz" = 0, "xx" = 0, "bb" = 2)
    )

    dfmat_conf2 <- dfm_match(dfmat, featnames(dfm(tokens("aa zz xx bb"))))
    expect_identical(
        featnames(dfmat_conf2),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(dfmat_conf2),
        c("doc1", "doc2")
    )
    expect_identical(
        colSums(dfmat_conf2),
        c("aa" = 2, "zz" = 0, "xx" = 0, "bb" = 2)
    )
    
    dfmat_conf3 <- dfm_match(dfmat, character())
    expect_identical(
        featnames(dfmat_conf3), character()
    )
    expect_identical(
        docnames(dfmat_conf3),
        c("doc1", "doc2")
    )
})

test_that("dfm_match works with padding", {
    toks <- tokens("aa bb !", padding = TRUE, remove_punct = TRUE)
    dfmat <- dfm(toks)
    expect_identical(
        featnames(dfm_match(dfmat, c("aa", "bb", "cc", ""))),
        c("aa", "bb", "cc", "")
    )
})

test_that("dfm_match coerce non-character feature", {
    txt <- c(doc1 = "TRUE TRUE FALSE",
             doc2 = "1 2 100")
    dfmat <- dfm(tokens(txt), tolower = FALSE)
    expect_equal(featnames(dfm_match(dfmat, c(TRUE, FALSE))),
                 c("TRUE", "FALSE"))
    expect_equal(featnames(dfm_match(dfmat, c(100, 1))),
                 c("100", 1))

})

test_that("dfm_match verbose works", {
    expect_message(
        dfm_match(data_dfm_lbgexample, c("B", "newfeat1", "A", "newfeat2"), verbose = TRUE),
        "dfm_match() changed from 37 features (6 documents) to 4 features (6 documents)",
        fixed = TRUE
    )
})
