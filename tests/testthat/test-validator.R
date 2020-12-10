context("test validator")

n1 <- 3.1
n2 <- -1.2
n3 <- c(-1.1, 0, 0.9, 1.0)
n4 <- c(1.0, NA)

c1 <- "abc"
c2 <- ""
c3 <- c("1", "abc", "abcdefg")
c4 <- c("z", NA)

l1 <- TRUE
l2 <- FALSE
l3 <- c(TRUE, FALSE, TRUE)
l4 <- c(TRUE, NA)

test_that("check_integer works", {

    expect_identical(quanteda:::check_integer(n1), 3L)
    expect_error(quanteda:::check_integer(c1), 
                 "c1 must be coercible to integer")
    expect_error(quanteda:::check_integer(n1, max = 2), 
                 "The value of n1 must be between -Inf and 2")
    expect_error(quanteda:::check_integer(n1, min = 5), 
                 "The value of n1 must be between 5 and Inf")
    expect_identical(quanteda:::check_integer(n2, min = -2), -1L)
    expect_error(quanteda:::check_integer(n2, max = -2), 
                 "The value of n2 must be between -Inf and -2")
    expect_error(quanteda:::check_integer(n2, min = 0), 
                 "The value of n2 must be between 0 and Inf")
    expect_error(quanteda:::check_integer(n2, len_max = 2, len_min = 2),
                 "The length of n2 must be 2")
    expect_error(quanteda:::check_integer(n3),
                 "The length of n3 must be 1")
    expect_error(quanteda:::check_integer(n3, len_max = 2),
                 "The length of n3 must be between 1 and 2")
    expect_error(quanteda:::check_integer(n4, len_max = 2), 
                 "The value of n4 cannot be NA")
})

test_that("check_double works", {
    
    expect_identical(quanteda:::check_double(n1), 3.1)
    expect_error(quanteda:::check_double(c1), 
                 "c1 must be coercible to double")
    expect_error(quanteda:::check_double(n1, max = 2), 
                 "The value of n1 must be between -Inf and 2")
    expect_error(quanteda:::check_double(n1, min = 5), 
                 "The value of n1 must be between 5 and Inf")
    expect_identical(quanteda:::check_double(n2, min = -2), -1.2)
    expect_error(quanteda:::check_double(n2, max = -2), 
                 "The value of n2 must be between -Inf and -2")
    expect_error(quanteda:::check_double(n2, min = 0), 
                 "The value of n2 must be between 0 and Inf")
    expect_error(quanteda:::check_double(n2, len_max = 2, len_min = 2),
                 "The length of n2 must be 2")
    expect_error(quanteda:::check_double(n3),
                 "The length of n3 must be 1")
    expect_error(quanteda:::check_double(n3, len_max = 2),
                 "The length of n3 must be between 1 and 2")
    expect_error(quanteda:::check_double(n4, len_max = 2), 
                 "The value of n4 cannot be NA")
})


test_that("check_character works", {
    
    expect_identical(quanteda:::check_character(n1), "3.1")
    expect_identical(quanteda:::check_character(c1), "abc")
    expect_identical(quanteda:::check_character(n2), "-1.2")
    expect_identical(quanteda:::check_character(c2), "")
    expect_error(quanteda:::check_character(c2, len_max = 2, len_min = 2),
                 "The length of c2 must be 2")
    expect_error(quanteda:::check_character(c3),
                 "The length of c3 must be 1")
    expect_error(quanteda:::check_character(c3, len_max = 2),
                 "The length of c3 must be between 1 and 2")
    expect_error(quanteda:::check_character(c3, nchar_max = 2, len_max = 3),
                 "The value of c3 must be between 0 and 2 character")
    expect_error(quanteda:::check_character(c3, nchar_min = 2, nchar_max = 2, len_max = 3),
                 "The value of c3 must be 2 character")
    expect_error(quanteda:::check_character(c4, len_max = 2), 
                 "The value of c4 cannot be NA")
})

test_that("check_logical works", {
    
    expect_identical(quanteda:::check_logical(n1), TRUE)
    expect_identical(quanteda:::check_logical(n2), TRUE)
    expect_identical(quanteda:::check_logical(n3, len_max = 4), 
                     c(TRUE, FALSE, TRUE, TRUE))
    expect_error(quanteda:::check_logical(c1), 
                     "The value of c1 cannot be NA")
    expect_error(quanteda:::check_logical(c2), 
                     "The value of c2 cannot be NA")
    expect_error(quanteda:::check_logical(l2, len_max = 2, len_min = 2),
                 "The length of l2 must be 2")
    expect_error(quanteda:::check_logical(l3),
                 "The length of l3 must be 1")
    expect_error(quanteda:::check_logical(l3, len_max = 2),
                 "The length of l3 must be between 1 and 2")
    expect_error(quanteda:::check_logical(l4, len_max = 2), 
                 "The value of l4 cannot be NA")
})