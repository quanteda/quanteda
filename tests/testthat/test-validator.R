i1 <- 3L
i2 <- -1L
i3 <- c(-1L, 0L, 1L, 2L)
i4 <- c(1L, NA)

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

fun <- function(x){}
x1 <- NULL

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
    expect_error(quanteda:::check_integer(n2, max_len = 2, min_len = 2),
                 "The length of n2 must be 2")
    expect_error(quanteda:::check_integer(n3),
                 "The length of n3 must be 1")
    expect_error(quanteda:::check_integer(n3, max_len = 2),
                 "The length of n3 must be between 1 and 2")
    expect_error(quanteda:::check_integer(n4, max_len = 2), 
                 "The value of n4 cannot be NA")
    expect_error(quanteda:::check_integer(fun), 
                 "fun must be coercible to integer")
    expect_error(quanteda:::check_integer(n1, strict = TRUE), 
                 "The type of n1 must be integer")
    expect_error(quanteda:::check_integer(x1), 
                 "x1 cannot be NULL")
    expect_true(is.null(quanteda:::check_integer(x1, allow_null = TRUE)))
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
    expect_error(quanteda:::check_double(n2, max_len = 2, min_len = 2),
                 "The length of n2 must be 2")
    expect_error(quanteda:::check_double(n3),
                 "The length of n3 must be 1")
    expect_error(quanteda:::check_double(n3, max_len = 2),
                 "The length of n3 must be between 1 and 2")
    expect_error(quanteda:::check_double(n4, max_len = 2), 
                 "The value of n4 cannot be NA")
    expect_error(quanteda:::check_double(fun), 
                 "fun must be coercible to double")
    expect_error(quanteda:::check_double(i1, strict = TRUE), 
                 "The type of i1 must be double")
    expect_error(quanteda:::check_double(x1), 
                 "x1 cannot be NULL")
    expect_true(is.null(quanteda:::check_double(x1, allow_null = TRUE)))
})


test_that("check_character works", {
    
    expect_identical(quanteda:::check_character(n1), "3.1")
    expect_identical(quanteda:::check_character(c1), "abc")
    expect_identical(quanteda:::check_character(n2), "-1.2")
    expect_identical(quanteda:::check_character(c2), "")
    expect_error(quanteda:::check_character(c2, max_len = 2, min_len = 2),
                 "The length of c2 must be 2")
    expect_error(quanteda:::check_character(c3),
                 "The length of c3 must be 1")
    expect_error(quanteda:::check_character(c3, max_len = 2),
                 "The length of c3 must be between 1 and 2")
    expect_error(quanteda:::check_character(c3, max_nchar = 2, max_len = 3),
                 "The value of c3 must be between 0 and 2 character")
    expect_error(quanteda:::check_character(c3, min_nchar = 2, max_nchar = 2, max_len = 3),
                 "The value of c3 must be 2 character")
    expect_error(quanteda:::check_character(c4, max_len = 2), 
                 "The value of c4 cannot be NA")
    expect_error(quanteda:::check_character(fun), 
                 "fun must be coercible to character")
    expect_error(quanteda:::check_character(n1, strict = TRUE), 
                 "The type of n1 must be character")
    expect_error(quanteda:::check_character(x1), 
                 "x1 cannot be NULL")
    expect_true(is.null(quanteda:::check_character(x1, allow_null = TRUE)))
})

test_that("check_logical works", {
    
    expect_identical(quanteda:::check_logical(n1), TRUE)
    expect_identical(quanteda:::check_logical(n2), TRUE)
    expect_identical(quanteda:::check_logical(n3, max_len = 4), 
                     c(TRUE, FALSE, TRUE, TRUE))
    expect_identical(quanteda:::check_logical(l4, max_len = 2, allow_na = TRUE), 
                     c(TRUE, FALSE))
    expect_error(quanteda:::check_logical(c1), 
                     "The value of c1 cannot be NA")
    expect_error(quanteda:::check_logical(c2), 
                     "The value of c2 cannot be NA")
    expect_error(quanteda:::check_logical(l2, max_len = 2, min_len = 2),
                 "The length of l2 must be 2")
    expect_error(quanteda:::check_logical(l3),
                 "The length of l3 must be 1")
    expect_error(quanteda:::check_logical(l3, max_len = 2),
                 "The length of l3 must be between 1 and 2")
    expect_error(quanteda:::check_logical(l4, max_len = 2), 
                 "The value of l4 cannot be NA")
    expect_error(quanteda:::check_logical(fun), 
                 "fun must be coercible to logical")
    expect_error(quanteda:::check_logical(i1, strict = TRUE), 
                 "The type of i1 must be logical")
    expect_error(quanteda:::check_logical(x1), 
                 "x1 cannot be NULL")
    expect_true(is.null(quanteda:::check_logical(x1, allow_null = TRUE)))
})


test_that("check_class works", {
    expect_silent(
        quanteda:::check_class("dfm", "dfm_select")
    )
    expect_silent(
        quanteda:::check_class(c("dfm", "Matrix"), "dfm_select")
    )
    expect_error(
        quanteda:::check_class("character", "dfm_select"), 
        "dfm_select() only works on dfm objects.", fixed = TRUE
    )
    expect_error(
        quanteda:::check_class(c("corpus", "character"), "dfm_select"), 
        "dfm_select() only works on dfm objects.", fixed = TRUE
    )
    expect_error(
        quanteda:::check_class("dfm", "tokens"), 
        "tokens() only works on character, corpus, list, tokens, tokens_xptr objects.", fixed = TRUE
    )
})

test_that("check_dots works", {
    fun1 <- function(...) quanteda:::check_dots(..., method = "tokens")
    expect_warning(
        fun1(tolower = TRUE),
        "tolower argument is not used.", fixed = TRUE
    )
    expect_warning(
        fun1(tolower = TRUE, 123, list()),
        "tolower argument is not used.", fixed = TRUE
    )
    
    fun2 <- function(...) quanteda:::check_dots(..., method = c("tokens", "corpus"))
    expect_warning(
        fun2(tolower = TRUE),
        "tolower argument is not used.", fixed = TRUE
    )
    
    fun3 <- function(...) quanteda:::check_dots(..., method =  c("tokens", "corpus", "dfm"))
    expect_silent(
        fun3(tolower = TRUE)
    )
})

