test_that("test ntoken on sentences", {
    txt <- c(doc1 = "This is Mr. Smith.  He is married to Mrs. Jones.",
             doc2 = "Never, before: a colon!  Gimme a break.")
    expect_identical(
        ntoken(tokens(txt, what = "sentence")),
        c(doc1 = 2L, doc2 = 2L)
    )
})

test_that("test ntype with dfm (#748)", {
    
    txt <- c(d1 = "a b c a b c", 
             d2 = "a b c d e",
             d3 = "")
    corp <- corpus(txt)
    toks <- tokens(corp)
    dfmt <- dfm(toks)
    dfmt2 <- dfm_remove(dfmt, "a", padding = TRUE)
    
    expect_identical(ntype(dfmt, remove_padding = TRUE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_identical(ntype(dfmt, remove_padding = FALSE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_identical(ntype(dfmt2, remove_padding = TRUE), c(d1 = 2L, d2 = 4L, d3 = 0L))
    expect_identical(ntype(dfmt2, remove_padding = FALSE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_error(
        ntype(dfmt, remove_padding = c(TRUE, FALSE)),
        "The length of remove_padding must be 1"
    )
})

# test_that("cannot call ntoken on a weighted dfm", {
#     d <- dfm(c(doc1 = "one two three", doc2 = "one one one")) |>
#         dfm_weight(scheme = "prop")
#     expect_error(
#         ntoken(d),
#         "cannot count the tokens in a weighted dfm - use colSums\\(\\) instead"
#     )
# })

test_that("test ntoken.tokens", {
    txt <- c(d1 = "a b c a b c", 
             d2 = "a b c d e",
             d3 = "")
    corp <- corpus(txt)
    toks <- tokens(corp)
    toks2 <- tokens_remove(toks, "a", padding = TRUE)
    
    expect_identical(ntoken(toks), c(d1 = 6L, d2 = 5L, d3 = 0L))
    expect_identical(ntoken(toks, remove_padding = TRUE), c(d1 = 6L, d2 = 5L, d3 = 0L))
    expect_identical(ntoken(toks, remove_padding = FALSE), c(d1 = 6L, d2 = 5L, d3 = 0L))
    expect_identical(ntoken(toks2, remove_padding = TRUE), c(d1 = 4L, d2 = 4L, d3 = 0L))
    expect_identical(ntoken(toks2, remove_padding = FALSE), c(d1 = 6L, d2 = 5L, d3 = 0L))
    expect_error(
        ntoken(toks2, remove_padding = c(TRUE, FALSE)),
        "The length of remove_padding must be 1"
    )
})

test_that("test ntype.tokens", {
    txt <- c(d1 = "a b c a b c", 
             d2 = "a b c d e",
             d3 = "")
    corp <- corpus(txt)
    toks <- tokens(corp)
    toks2 <- tokens_remove(toks, "a", padding = TRUE)
    
    expect_identical(ntype(toks), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_identical(ntype(toks, remove_padding = TRUE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_identical(ntype(toks, remove_padding = FALSE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_identical(ntype(toks2, remove_padding = TRUE), c(d1 = 2L, d2 = 4L, d3 = 0L))
    expect_identical(ntype(toks2, remove_padding = FALSE), c(d1 = 3L, d2 = 5L, d3 = 0L))
    expect_error(
        ntype(toks2, remove_padding = c(TRUE, FALSE)),
        "The length of remove_padding must be 1"
    )
})

test_that("dots are applied in ntokens.tokens, ntype.tokens", {
    txt <- c(d1 = "3 wonderful tokens of the tokens function.")
    toks <- tokens(txt)

    expect_identical(ntoken(toks), c(d1 = 8L))
    expect_identical(ntoken(toks, remove_punct = TRUE), c(d1 = 7L))
    expect_identical(ntoken(toks, remove_punct = TRUE, remove_numbers = TRUE), c(d1 = 6L))
    expect_warning(ntoken(toks, notarg = TRUE), "^notarg argument is not used")

    expect_identical(ntype(toks), c(d1 = 7L))
    expect_identical(ntype(toks, remove_punct = TRUE), c(d1 = 6L))
    expect_identical(ntype(toks, remove_punct = TRUE, remove_numbers = TRUE), c(d1 = 5L))
    expect_warning(ntype(toks, notarg = TRUE), "^notarg argument is not used")

    suppressWarnings(expect_identical(ntype(txt, remove_punct = TRUE), c(d1 = 6L)))
    expect_identical(ntype(txt), c(d1 = 7L))
})

test_that("test nsentence", {
    txt <- c(doc1 = "This is Mr. Smith.  He is married to Mrs. Jones.",
             doc2 = "Never, before: a colon!  Gimme a break.")
    suppressWarnings(expect_identical(nsentence(txt), c(doc1 = 2L, doc2 = 2L)))
    expect_identical(nsentence(corpus(txt)), c(doc1 = 2L, doc2 = 2L))
    expect_identical(
        nsentence(tokens(txt, what = "sentence")),
        c(doc1 = 2L, doc2 = 2L)
    )
})

test_that("nsentence warnings work", {
    txt <- c(d1 = "one two three")
    expect_warning(
        nsentence(txt),
        "nsentence() does not correctly count sentences in all lower-cased text",
        fixed = TRUE
    )
    expect_warning(
        nsentence(corpus(txt)),
        "nsentence() does not correctly count sentences in all lower-cased text",
        fixed = TRUE
    )
})
