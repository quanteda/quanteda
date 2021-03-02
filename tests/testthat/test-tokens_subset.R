test_that("tokens_subset works in a basic way", {
    toks <- tokens(corpus_subset(data_corpus_inaugural, Year > 1980 & Year < 2018))
    expect_equal(
        ndoc(tokens_subset(toks, Year > 2000)),
        5
    )
    expect_equal(
        length(levels(docid(tokens_subset(toks, Year > 2000, drop_docid = TRUE)))),
        5
    )
    expect_equal(
        length(levels(docid(tokens_subset(toks, Year > 2000, drop_docid = FALSE)))), 
        10
    )
    expect_equal(
        docnames(tokens_subset(toks, President == "Clinton")),
        c("1993-Clinton", "1997-Clinton")
    )
    expect_equal(
        docnames(tokens_subset(toks, c(TRUE, TRUE, rep(FALSE, 8)))),
        c("1981-Reagan", "1985-Reagan")
    )
    expect_warning(
        tokens_subset(toks, Year > 2000, something = 10),
        "something argument is not used.", fixed = TRUE
    )
})

test_that("tokens_subset works with docvars", {
    toks <- tokens(corpus_subset(data_corpus_inaugural, Year > 1900))
    expect_equal(
        docvars(head(toks, 5))$President,
        c("McKinley", "Roosevelt", "Taft", "Wilson", "Wilson")
    )
})
