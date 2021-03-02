test_that("dfm_subset works in a basic way", {
    dfmat <- dfm(tokens(corpus_subset(data_corpus_inaugural, Year > 1980 & Year < 2018)))
    expect_equal(
        ndoc(dfm_subset(dfmat, Year > 2000)),
        5
    )
    expect_equal(
        length(levels(docid(dfm_subset(dfmat, Year > 2000, drop_docid = TRUE)))),
        5
    )
    expect_equal(
        length(levels(docid(dfm_subset(dfmat, Year > 2000, drop_docid = FALSE)))), 
        10
    )
    expect_equal(
        docnames(dfm_subset(dfmat, President == "Clinton")),
        c("1993-Clinton", "1997-Clinton")
    )
    expect_equal(
        docnames(dfm_subset(dfmat, c(TRUE, TRUE, rep(FALSE, 8)))),
        c("1981-Reagan", "1985-Reagan")
    )
    expect_warning(
        dfm_subset(dfmat, Year > 2000, something = 10),
        "something argument is not used.", fixed = TRUE
    )
})

test_that("dfm_subset works with docvars", {
    dfmat <- dfm(tokens(corpus_subset(data_corpus_inaugural, Year > 1900)))
    expect_equal(
        docvars(head(dfmat, 5))$President,
        c("McKinley", "Roosevelt", "Taft", "Wilson", "Wilson")
    )
})
