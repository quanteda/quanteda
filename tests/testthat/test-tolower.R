library(quanteda)

context("toLower")

test_that("toLower works.", {
    expect_equal(toLower("According to NATO"), "according to nato")
})

test_that("toLower keeps acronyms.", {
    expect_equal(toLower("According to NATO", keep_acronyms = TRUE), "according to NATO")
})

test_that("char_tolower works.", {
    expect_equal(char_tolower("According to NATO"), "according to nato")
})

test_that("char_tolower keeps acronyms.", {
    expect_equal(char_tolower("According to NATO", keep_acronyms = TRUE), "according to NATO")
})



