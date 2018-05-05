
context("test direction change functions")

# Note: the line below does not appear correctly in R Studio

test_that("char_tortl works.", {
    skip_on_os("windows")
    txt <- "מדינת ישראל נוסדה בשנת 1948."
    expect_equal(char_tortl(txt), "מדינת ישראל נוסדה בשנת 1948.\u200F")
    
    txt <- "!מדינת ישר computer וסדה בשנת 1948."
    # expect_equal(char_tortl(txt), "!מדינת ישר computer וסדה בשנת 1948.")
})

test_that("tokens_tortl works.", {
    skip_on_os("windows")
    txt <- "מדינת ישראל נוסדה בשנת 1948."
    toks <- tokens(txt)
    expect_equal(types(tokens_tortl(toks)), types(tokens(char_tortl(txt))))
})
