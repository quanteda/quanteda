context("test direction change functions")

# Note: the line below does not appear correctly in R Studio
txt <- "מדינת ישראל נוסדה בשנת 1948."

test_that("char_tortl works.", {
    expect_equal(char_tortl(txt), "מדינת ישראל נוסדה בשנת 1948.\u200F")
})
