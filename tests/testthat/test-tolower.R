library(quanteda)

context("toLower")

test_that("toLower works.", {
    expect_equal(suppressWarnings(toLower("According to NATO")), "according to nato")
})

test_that("toLower keeps acronyms.", {
    expect_equal(suppressWarnings(toLower("According to NATO", keep_acronyms = TRUE)), "according to NATO")
})

test_that("char_tolower/char_toUpper works.", {
    expect_equal(suppressWarnings(toLower("According to NATO")), "according to nato")
    expect_equal(suppressWarnings(toUpper("According to NATO")), "ACCORDING TO NATO")
})

test_that("char_tolower keeps acronyms.", {
    expect_equal(suppressWarnings(toLower("According to NATO", keep_acronyms = TRUE)), "according to NATO")
})

test_that("tokens_tolower/tokens_toupper works", {
    toksh <- tokens("According to NATO")
    expect_equal(suppressWarnings(as.list(toLower(toksh))[[1]]),
                      c("according", "to", "nato"))
    expect_equal(suppressWarnings(as.list(toUpper(toksh))[[1]]),
                 c("ACCORDING", "TO", "NATO"))
})

test_that("toLower/toUpper.tokenizedTexts works as expected", {
    toksh <- tokenize("According to NATO")
    expect_equal(suppressWarnings(as.list(toLower(toksh))[[1]]),
                 c("according", "to", "nato"))
    expect_equal(suppressWarnings(as.list(toUpper(toksh))[[1]]),
                 c("ACCORDING", "TO", "NATO"))
})

test_that("toLower/toUpper.corpus works as expected", {
    corp <- corpus("According to NATO")
    expect_equal(suppressWarnings(toLower(corp))[[1]],
                 c("according to nato"))
    expect_equal(suppressWarnings(toUpper(corp))[[1]],
                 c("ACCORDING TO NATO"))
})
