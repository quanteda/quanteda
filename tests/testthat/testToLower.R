library(quanteda)

context("toLower")

test_that("toLower works.", {
    expect_equal(toLower("According to NATO"), "according to nato")
    expect_equal(toLower("Утвержденной НАТО"), "утвержденной нато")
})

test_that("toLower keeps acronyms.", {
    expect_equal(toLower("According to NATO", keepAcronyms = TRUE), "according to NATO")
    expect_equal(toLower("Утвержденной НАТО", keepAcronyms = TRUE), "утвержденной НАТО")
})
