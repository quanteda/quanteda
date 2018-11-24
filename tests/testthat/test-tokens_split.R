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



