
test_that("char_wordstem works", {
    
    expect_equal(char_wordstem("coding", "porter"), "code")
    expect_equal(char_wordstem("coding", "english"), "code")
    
    txt <- c("code coding")
    expect_error(
        char_wordstem(txt),
        "whitespace detected: you can only stem tokenized texts"
    )
    expect_identical(
        char_wordstem(txt, check_whitespace = FALSE),
        "code cod"
    )
})

test_that("tokens_wordstem works", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem", "plural", "perfect"),
                      d2 = c("one", "two", "three")))
    expect_message(
        tokens_wordstem(toks, "english", verbose = TRUE),
        "tokens_wordstem() changed", fixed = TRUE
    )
})

test_that("tokens_wordstem works with ngrams", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt) |> tokens_ngrams(n = 2)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem_plural", "plural_perfect"),
                      d2 = c("one_two", "two_three")))
})

test_that("tokens_wordstem works with tokens with padding = TRUE", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens_remove(tokens(txt), c("one", "three"), padding = TRUE)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem", "plural", "perfect"),
                      d2 = c("", "two", "")))
})

test_that("tokens_wordstem works on tokens that include separators (#909)", {
    txt <- "Tests for developers."
    toks <- tokens(txt, remove_punct = TRUE)
    expect_equal(
        as.list(tokens_wordstem(toks, language = "english")),
        list(text1 = c("Test", "for", "develop"))
    )
})
