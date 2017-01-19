library(quanteda)

context("toLower")

test_that("toLower works.", {
    expect_equal(suppressWarnings(toLower("According to NATO")), "according to nato")
})

test_that("toLower keeps acronyms.", {
    expect_equal(suppressWarnings(toLower("According to NATO", keep_acronyms = TRUE)), "according to NATO")
})

test_that("char_tolower works.", {
    expect_equal(suppressWarnings(toLower("According to NATO")), "according to nato")
})

test_that("char_tolower keeps acronyms.", {
    expect_equal(suppressWarnings(toLower("According to NATO", keep_acronyms = TRUE)), "according to NATO")
})



