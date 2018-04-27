context("test direction change functions")

# Note: the line below does not appear correctly in R Studio
txt <- "מדינת ישראל נוסדה בשנת 1948."
toks <- tokens(txt)

test_that("char_tortl works.", {
    expect_equal(char_tortl(txt), "מדינת ישראל נוסדה בשנת 1948.\u200F")
})

test_that("tokens_tortl works.", {
    expect_equal(types(tokens_tortl(toks)), types(tokens(char_tortl(txt))))
})
