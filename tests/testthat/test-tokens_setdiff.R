context("test tokens_setdiff")

test_that("test tokens_setdiff", {
    
    toks <- tokens(c(d1 = "This sentence is extremely brief.",
                     d2 = "Two heads are superior to one."), remove_punct = TRUE)
    expect_identical(
        tokens_setdiff(tokens_tolower(toks), pattern = data_char_wordlists$dalechall),
        as.tokens(list(d1 = c("extremely", "brief"), d2 = "superior"))
    )
    
    txt <- c(d1 = "a a b c d", d2 = "x y c c")
    expect_identical(
        tokens_setdiff(tokens(txt), c("c", "d")),
        as.tokens(list(d1 = c("a", "a", "b"), d2 = c("x", "y")))
    )
    expect_identical(
        tokens_setdiff(tokens(txt), c("c", "d"), unique = TRUE),
        as.tokens(list(d1 = c("a", "b"), d2 = c("x", "y")))
    )
    
    expect_error(
        tokens_setdiff(tokens(txt), c("c", "d"), valuetype = "glob"),
        "glob and regex patterns are not yet implemented for this function"
    )

    expect_error(
        tokens_setdiff(tokens(txt), c("c", "d"), valuetype = "regex"),
        "glob and regex patterns are not yet implemented for this function"
    )
    
    expect_identical(
        tokens_setdiff(tokens(txt), c("C", "d"), case_insensitive = TRUE),
        tokens_setdiff(tokens(txt), c("c", "d"), case_insensitive = FALSE)
    )
    
    txt <- c(doc1 = "quanteda's functions the masses' choice")
    toks <- tokens(txt, what = "fasterword")
    expect_identical(
        tokens_setdiff(toks, c("quanteda's")),
        as.tokens(list(doc1 = c("functions", "the", "masses'", "choice")))
    )
})
