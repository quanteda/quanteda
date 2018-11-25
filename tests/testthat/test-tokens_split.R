context("test tokens_split")

test_that("tokens_split works", {
    toks <- tokens("a-a b+b B*B cc DD ee", what = "fastestword")
    expect_equal(as.list(tokens_split(toks, separator = "-")),
                 list(text1 = c("a", "-", "a", "b+b", "B*B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "-", remove_separator = TRUE)),
                 list(text1 = c("a", "a", "b+b", "B*B", "cc", "DD", "ee")))
    
    expect_equal(as.list(tokens_split(toks, separator = "+")),
                 list(text1 = c("a-a", "b", "+", "b", "B*B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "+", remove_separator = TRUE)),
                 list(text1 = c("a-a", "b", "b", "B*B", "cc", "DD", "ee")))
    
    expect_equal(as.list(tokens_split(toks, separator = "\\p{P}", valuetype = "regex")),
                 list(text1 = c("a", "-", "a", "b+b", "B", "*", "B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "\\p{S}", valuetype = "regex")),
                 list(text1 = c("a-a", "b", "+", "b", "B*B", "cc", "DD", "ee")))
    
})

test_that("tokens_split error when more than one separator is given", {
    toks <- tokens("a-a b+b B*B cc DD ee", what = "fastestword")
    expect_error(tokens_split(toks, c("-", "+")),
                 "separator must be a character")
})

test_that("tokens_split works in the same way as tokens", {
    txt <- "US-EU low-carbon agreement"
    expect_equal(
        as.list(tokens(txt, remove_hyphens = TRUE)),
        as.list(tokens_split(tokens(txt, what = "fastestword"), "\\p{P}", valuetype = "regex"))
    )
})

test_that("tokens_split works fully when matching entire tokens to separator pattern", {
    toks <- tokens("five-star rating", remove_hyphen = TRUE)
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "regex", remove_separator = FALSE)),
        c("five", "-", "star", "rating")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "regex", remove_separator = TRUE)),
        c("five", "star", "rating")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "fixed", remove_separator = TRUE)),
        c("five", "star", "rating")
    )
})

test_that("tokens_split works with multi-character separator", {
    skip("Pending whether @koheiw thinks this should be a test")
    toks <- tokens("five--star rating", remove_hyphen = TRUE)
    expect_identical(
        as.character(tokens_split(toks, separator = "--", remove_separator = FALSE)),
        c("five", "--", "star", "rating")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "--", remove_separator = TRUE)),
        c("five", "star", "rating")
    )
})

test_that("tokens_split works with regex character separator of indeterminate pattern", {
    skip("Pending whether @koheiw thinks this should be a test")
    toks <- tokens("abcdef")
    expect_identical(
        as.character(tokens_split(toks, separator = "[be]", valuetype = "regex", remove_separator = FALSE)),
        c("a", "b", "cd", "e", "f")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "[be]", valuetype = "regex", remove_separator = TRUE)),
        c("a", "cd", "f")
    )
})
