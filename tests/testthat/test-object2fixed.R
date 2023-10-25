test_that("object2id is working with a list", {
    
    pat <- c('A', 'a b', 'xxx', 'C D', 'e f g')
    ids1 <- quanteda:::object2id(phrase(pat), types = letters, 
                                 valuetype = 'fixed', case_insensitive = TRUE)
    lis1 <- list("A" = 1, "a b" = c(1, 2), "C D" = c(3, 4),
                 "e f g" = c(5, 6, 7))
    attr(lis1, "pattern") <- c(1, 2, 4, 5)
    expect_equal(ids1, lis1)
    
    ids2 <- quanteda:::object2id(phrase(pat), types = letters, 
                                 valuetype = 'fixed', case_insensitive = FALSE)
    lis2 <- list("a b" = c(1, 2), "e f g" = c(5, 6, 7))
    attr(lis2, "pattern") <- c(2, 5)
    expect_equal(ids2, lis2)
    
    ids3 <- quanteda:::object2id(phrase(pat), types = letters, 
                                 valuetype = 'fixed', case_insensitive = FALSE,
                                 keep_nomatch = TRUE)
    lis3 <- list("A" = integer(), "a b" = c(1, 2), "xxx" = integer(), "C D" = integer(),
                 "e f g" = c(5, 6, 7))
    attr(lis3, "pattern") <- c(1, 2, 3, 4, 5)
    expect_equal(ids3, lis3)
    
})

test_that("object2id is working with a list", {
    
    pat <- c('a', 'a b', 'c d', 'e f g')
    fps <- quanteda:::object2fixed(phrase(pat), types = letters, 
                                   valuetype = 'fixed', case_insensitive = FALSE)
    lis <-  list("a" = "a", "a b" = c("a", "b"), "c d" = c("c", "d"),
                 "e f g" = c("e", "f", "g"))
    expect_equal(fps, lis)
})

test_that("object2id is working with a dictionary", {
    type <- c("ab", "bb", "a a", "a b")
    dict <- dictionary(list(key1 = c("a*", "b*"), key2 = c("a a", "a b")))
    ids1 <- quanteda:::object2id(dict, type, valuetype = "glob", case_insensitive = FALSE)
    lis1 <- list("key1" = 1, "key1" = 3, "key1" = 4, "key1" = 2)
    attr(lis1, "pattern") <- c(1, 1, 1, 1)
    attr(lis1, "key") <- c("key1", "key2")
    expect_equal(ids1, lis1)
    
    ids2 <- quanteda:::object2id(dict, type, valuetype = "glob", case_insensitive = FALSE,
                                 concatenator = " ")
    lis2 <- list("key1" = 1, "key1" = 3, "key1" = 4, "key1" = 2, 
                 "key2" = 3, "key2" = 4)
    attr(lis2, "pattern") <- c(1, 1, 1, 1, 3, 3)
    attr(lis2, "key") <- c("key1", "key2")
    expect_equal(ids2, lis2)
})

test_that("object2id is working with a dictionary", {
    
    load("../data/collocations/coll_bi.rda")
    
    type <- attr(coll_bi, "types")
    ids1 <- quanteda:::object2id(coll_bi, type, case_insensitive = FALSE)
    lis1 <- list("a b" = c(1, 2), "e g" = c(5, 6), "g h" = c(6, 7))
    attr(lis1, "pattern") <- c(1, 2, 3)
    expect_equal(ids1, lis1)
    
    ids2 <- quanteda:::object2id(coll_bi, type, case_insensitive = TRUE)
    lis2 <- list("a b" = c(1, 2), "e g" = c(5, 6), "g h" = c(6, 7))
    attr(lis2, "pattern") <- c(1, 2, 3)
    expect_equal(ids2, lis1)
})

test_that("object2id is working with empty patterns", {
    col <- data.frame()
    class(col) <- c("collocations", "data.frame")
    pat <- list()
    expect_silent(quanteda:::object2id(col, letters, "fixed", TRUE))
    expect_silent(quanteda:::object2id(pat, letters, "fixed", TRUE))
})

test_that("object2id error if x is not characters", {
    expect_error(
        quanteda:::object2id(c(TRUE, FALSE), letters),
        "The type of x must be character"
    )
    
    expect_error(
        quanteda:::object2id(c(1, 2), letters),
        "The type of x must be character"
    )
    
    expect_error(
        quanteda:::object2id(list("a", 10), letters),
        "The type of x must be character"
    )
    
})

