context("test nfunctions")

test_that("test nsentence", {
    txt <- c(doc1 = "This is Mr. Smith.  He is married to Mrs. Jones.",
             doc2 = "Never, before: a colon!  Gimme a break.")
    expect_identical(nsentence(txt), c(doc1 = 2L, doc2 = 2L))
    expect_identical(nsentence(corpus(txt)), c(doc1 = 2L, doc2 = 2L))
    expect_identical(
        nsentence(tokens(txt, what = "sentence")),
        c(doc1 = 2L, doc2 = 2L)
    )
})

test_that("test ntype with dfm (#748)", {
    d <- dfm(c(doc1 = "one two three",
               doc2 = "one one one"))
    expect_identical(
        ntype(d),
        c(doc1 = 3L, doc2 = 1L)
    )
    expect_identical(
        ntoken(d),
        c(doc1 = 3L, doc2 = 3L)
    )
})

# test_that("cannot call ntoken on a weighted dfm", {
#     d <- dfm(c(doc1 = "one two three", doc2 = "one one one")) %>%
#         dfm_weight(scheme = "prop")
#     expect_error(
#         ntoken(d),
#         "cannot count the tokens in a weighted dfm - use colSums\\(\\) instead"
#     )
# })

test_that("test ntoken tokens", {
    txt <- c(d1 = "a b c a b c", 
             d2 = "a b c d e")
    crp <- corpus(txt)
    expect_identical(ntoken(txt), c(d1 = 6L, d2 = 5L))
    expect_identical(ntoken(crp), c(d1 = 6L, d2 = 5L))
})

test_that("test ntype tokens", {
    txt <- c(d1 = "a b c a b c", 
             d2 = "a b c d e")
    toks <- tokens(txt)
    expect_identical(ntype(toks), c(d1 = 3L, d2 = 5L))
    expect_identical(ntype(toks), c(d1 = 3L, d2 = 5L))
    
    toks2 <- tokens_remove(toks, 'a', padding = TRUE)
    expect_identical(ntype(toks2), c(d1 = 2L, d2 = 4L))
    expect_identical(ntype(toks2), c(d1 = 2L, d2 = 4L))
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
    
    expect_identical(ntype(txt, remove_punct = TRUE), c(d1 = 6L))
    expect_identical(ntype(txt), c(d1 = 7L))
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
