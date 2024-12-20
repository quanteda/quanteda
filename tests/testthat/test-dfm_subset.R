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

test_that("tokens_subset works with min_ntoken and max_ntoken", {
    toks <- tokens(corpus_subset(data_corpus_inaugural, Year > 1900))
    dfmt <- dfm(toks)
    
    expect_equal(
        dfm_subset(dfmt, 1000 <= ntoken(dfmt)),
        dfm_subset(dfmt, min_ntoken = 1000)
    )
    
    expect_equal(
        dfm_subset(dfmt, ntoken(dfmt) <= 3000),
        dfm_subset(dfmt, max_ntoken = 3000)
    )
    
    expect_equal(
        dfm_subset(dfmt, Year > 2000 & 1000 <= ntoken(dfmt) & ntoken(dfmt) >= 1000),
        dfm_subset(dfmt, Year > 2000, min_ntoken = 1000, max_ntoken = 3000)
    )
    
    expect_error(
        dfm_subset(dfmt, min_ntoken = -1),
        "The value of min_ntoken must be between 0 and Inf"
    )
    
    expect_error(
        dfm_subset(dfmt, min_ntoken = c(10, 20)),
        "The length of min_ntoken must be 1"
    )
    
    expect_error(
        dfm_subset(dfmt, max_ntoken = -1),
        "The value of max_ntoken must be between 0 and Inf"
    )
    
    expect_error(
        dfm_subset(dfmt, max_ntoken = c(10, 20)),
        "The length of max_ntoken must be 1"
    )
})

test_that("dfm_subset() works with verbose", {
    corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                     d3 = "b b c e", d4 = "e e f a b"),
                   docvars = data.frame(grp = c(1, 1, 2, 3)))
    dfmat <- dfm(tokens(corp))
    
    expect_message(
        dfm_subset(dfmat, grp > 1, verbose = TRUE),
        "dfm_subset() changed from 6 features (4 documents) to 6 features (2 documents)",
        fixed = TRUE
    )
    expect_message(
        dfm_subset(dfmat, c(TRUE, FALSE, FALSE, FALSE), verbose = TRUE),
        "dfm_subset() changed from 6 features (4 documents) to 6 features (1 documents)",
        fixed = TRUE
    )
})
