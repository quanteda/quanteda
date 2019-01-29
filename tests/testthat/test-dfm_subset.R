context("test dfm_subset")
        
test_that("dfm_subset works in a basic way", {
    dfmtest <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980 & Year < 2018))
    expect_equal(
        ndoc(dfm_subset(dfmtest, Year > 2000)),
        5
    )
    expect_equal(
        docnames(dfm_subset(dfmtest, President == "Clinton")),
        c("1993-Clinton", "1997-Clinton")
    )
    expect_equal(
        docnames(dfm_subset(dfmtest, c(TRUE, TRUE, rep(FALSE, 8)))),
        c("1981-Reagan", "1985-Reagan")
    )
})

test_that("dfm_subset works with docvars", {
    dfmtest <- dfm(corpus_subset(data_corpus_inaugural, Year > 1900))
    expect_equal(
        docvars(dfm_subset(dfmtest, Year > 2000))$President,
        c("Bush", "Bush", "Obama", "Obama", "Trump")
    )
})
