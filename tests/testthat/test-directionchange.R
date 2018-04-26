context("test direction change functions")

# Note: the line below does not appear correctly in R Studio
txt <- c("a b c d.") 

test_that("char_tortl works.", {
    expect_equal(char_tolower(txt), "a b c d.\U200F")
    utf8ToInt('a b c d.\U200F')
})
