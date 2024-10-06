test_that("tokens_split works", {
    toks <- tokens("a-a b+b B*B cc DD ee", what = "fastestword")
    expect_equal(as.list(tokens_split(toks, separator = "-", remove_separator = FALSE)),
                 list(text1 = c("a", "-", "a", "b+b", "B*B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "-", remove_separator = TRUE)),
                 list(text1 = c("a", "a", "b+b", "B*B", "cc", "DD", "ee")))

    expect_equal(as.list(tokens_split(toks, separator = "+", remove_separator = FALSE)),
                 list(text1 = c("a-a", "b", "+", "b", "B*B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "+", remove_separator = TRUE)),
                 list(text1 = c("a-a", "b", "b", "B*B", "cc", "DD", "ee")))

    expect_equal(as.list(tokens_split(toks, separator = "\\p{P}", valuetype = "regex",
                                      remove_separator = FALSE)),
                 list(text1 = c("a", "-", "a", "b+b", "B", "*", "B", "cc", "DD", "ee")))
    expect_equal(as.list(tokens_split(toks, separator = "\\p{S}", valuetype = "regex",
                                      remove_separator = FALSE)),
                 list(text1 = c("a-a", "b", "+", "b", "B*B", "cc", "DD", "ee")))
    
    expect_message(
        tokens_split(toks, separator = "-", verbose = TRUE),
        "tokens_split() changed", fixed = TRUE
    )

})

test_that("tokens_split error when more than one separator is given", {
    toks <- tokens("a-a b+b B*B cc DD ee", what = "fastestword")
    expect_error(tokens_split(toks, c("-", "+")),
                 "The length of separator must be 1")
})

test_that("tokens_split works in the same way as tokens", {
    txt <- "US-EU low-carbon agreement"
    expect_equal(
        as.list(tokens(txt, split_hyphens = TRUE)),
        as.list(tokens_split(tokens(txt, what = "fastestword"), "\\p{P}", valuetype = "regex",
                             remove_separator = FALSE))
    )
})

test_that("tokens_split works fully when matching entire tokens to separator pattern", {
    toks <- tokens("five-star rating", split_hyphens = TRUE)
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "regex",
                                  remove_separator = FALSE)),
        c("five", "-", "star", "rating")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "regex",
                                  remove_separator = TRUE)),
        c("five", "star", "rating")
    )
    expect_identical(
        as.character(tokens_split(toks, separator = "-", valuetype = "fixed",
                                  remove_separator = TRUE)),
        c("five", "star", "rating")
    )
})


test_that("apply_if argument is working", {
    dat <- data.frame(text = c("US-EU low-carbon agreement",
                               "five-star hotel"),
                      topic = c("environment", "travel"))
    corp <- corpus(dat)
    toks <- tokens(corp)

    toks1 <- tokens_split(toks, separator = "-", apply_if = toks$topic == "environment")
    expect_identical(
        as.list(toks1),
        list(text1 = c("US", "EU", "low", "carbon", "agreement"),
             text2 = c("five-star", "hotel"))
    )

    toks2 <- tokens_split(toks, separator = "-", apply_if = toks$topic == "travel")
    expect_identical(
        as.list(toks2),
        list(text1 = c("US-EU", "low-carbon", "agreement"),
             text2 = c("five", "star", "hotel"))
    )
})
