context("tests for nscrabble")

test_that("test nscrabble", {
    txt1 <- c("muzjiks", "excellency", "")
    txt2 <- c(d1 = "muzjiks", d2 = "excellency", d3 = "")
    
    expect_identical(nscrabble(txt1), c(29L, 24L, NA))
    expect_identical(nscrabble(txt2), c(d1 = 29L, d2 = 24L, d3 = NA))
})
