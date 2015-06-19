library(quanteda)

test_that("simple wordstem test to test testing.", {
    expect_equal(wordstem('testing'), 'test')
})