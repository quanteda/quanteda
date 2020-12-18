context("test utils")

test_that("message_select works as expected", {
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        "removed 10 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        " and 5 documents"
    )
    
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 0),
        "removed 1 feature"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 0, 0),
        " and 1 document$"
    )
    expect_message(
        quanteda:::message_select("remove", 0, 0, 0, 0),
        "removed 0 features$"
    )

    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        "removed 1,000 features"
    )
    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        " and 1,000,000 documents$"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "2 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "3 documents"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        ", padded"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        "1 document"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 1, 0),
        "1 feature"
    )
})

test_that("pipes work", {
    expect_true(!"package:magrittr" %in% search())
    expect_equal(
        tokens(char_tolower("A B C")),
        tokens("A B C") %>% tokens_tolower()
    )
})

test_that("pattern2list is working with a list", {
    type <- letters
    pat <- c('a', 'a b', 'c d', 'e f g')
    ids <- quanteda:::pattern2list(phrase(pat), type, 'fixed', FALSE)
    expect_equal(names(ids), pat)
})

test_that("pattern2list is working with a dictionary", {
    type <- c("a", "ab", "b", "bb", "a a", "a b")
    dict <- dictionary(list(key1 = c("a*", "b*"), key2 = c("a a*", "a b*")))
    ids <- quanteda:::pattern2list(dict, type, "glob", FALSE)
    expect_true(all(names(dict) %in% names(ids)))
})

test_that("pattern2list is working with empty patterns", {
    col <- data.frame()
    class(col) <- c("collocations", "data.frame")
    pat <- list()
    expect_silent(quanteda:::pattern2list(col, types(toks), "fixed", TRUE))
    expect_silent(quanteda:::pattern2list(pat, types(toks), "fixed", TRUE))
})

test_that("get_package_version works", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    expect_true(quanteda:::get_object_version(data_corpus_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_corpus_pre2))
    expect_true(quanteda:::get_object_version(corpus("one")) > "1.4.9")
    
    load("../data/pre_v2_objects/data_tokens_pre2.rda")
    expect_true(quanteda:::get_object_version(data_tokens_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_tokens_pre2))
    expect_true(quanteda:::get_object_version(tokens("one")) > "1.4.9")
    
    load("../data/pre_v2_objects/data_dfm_pre2.rda")
    expect_true(quanteda:::get_object_version(data_dfm_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_dfm_pre2))
    expect_true(quanteda:::get_object_version(dfm("one")) > "1.4.9")
})

test_that("resample works with sizes", {
    grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
    
    # resample works without group 
    tmp <- quanteda:::resample(1:10, size = 5, replace = FALSE)
    expect_equal(length(tmp), 5)
    
    tmp <- quanteda:::resample(1:10, size = 20, replace = TRUE)
    expect_equal(length(tmp), 20)

    expect_error(quanteda:::resample(1:10, size = 20, replace = FALSE),
                 "size cannot exceed the number of items when replace = FALSE")
    
    # sampling same size each group
    tmp <- quanteda:::resample(1:10, size = 2, replace = TRUE, by = grvec)
    expect_true(all(tmp[1:2] >= 0 & tmp[1:2] <= 3))
    expect_true(all(tmp[3:4] >= 4 & tmp[3:4] <= 7))
    expect_true(all(tmp[5:6] >= 8 & tmp[5:6] <= 10))
    
    # sampling from a vector of sizes
    tmp <- quanteda:::resample(1:10, size = c(1, 1, 3), replace = TRUE, by = grvec)
    expect_true(all(tmp[1] >= 0 & tmp[1] <= 3))
    expect_true(all(tmp[2] >= 4 & tmp[2] <= 7))
    expect_true(all(tmp[3:5] >= 8 & tmp[3:5] <= 10))
    
    # default group size sampling
    tmp <- quanteda:::resample(1:10, replace = TRUE, by = grvec)
    expect_true(all(tmp[1:3] >= 0 & tmp[1:3] <= 3))
    expect_true(all(tmp[4:7] >= 4 & tmp[4:7] <= 7))
    expect_true(all(tmp[8:10] >= 8 & tmp[8:10] <= 10))
    
    # sampling with prob
    tmp <- quanteda:::resample(1:2, 10, prob = c(1, 0), replace = TRUE)
    expect_equal(tmp, rep(1, 10))
    
    # oversampling within group
    tmp <- quanteda:::resample(c(1:2, c(101:102)), size = 5, replace = TRUE, 
                               by = rep(letters[1:2], each = 2))
    expect_true(all(tmp[1:5] >= 0 & tmp[1:5] <= 2))
    expect_true(all(tmp[6:10] >= 100 & tmp[6:10] <= 102))
    
    expect_error(
        quanteda:::resample(1:10, size = c(2, 3), replace = TRUE, by = grvec[1:6]),
        "x and by must have the same length"
    )
    expect_error(
        quanteda:::resample(1:10, size = c(2, 3), replace = TRUE, by = grvec),
        "size and by must have the same length"
    )
    expect_error(
        quanteda:::resample(1:10, size = 5, replace = FALSE, by = grvec),
        "size cannot exceed the number of items within group when replace = FALSE"
    )
    expect_error(
        quanteda:::resample(1:10, 10, prob = rep(0.1, 10), replace = TRUE, by = grvec),
        "prob cannot be used with by"
    )
})

test_that("resample works with groups of size 1", {
    grvec <- c(rep("a", 3), rep("b", 1))
    for (i in seq_len(10)) {
        tmp <- quanteda:::resample(1:4, replace = TRUE, by = grvec)
        expect_true(all(tmp[1:3] >= 1 & tmp[1:3] <= 3))
        expect_equal(tmp[4], 4)
    }
})

test_that("max_load_factor can be configured", {
    quanteda:::qatd_cpp_set_load_factor("pattern", 0.2)
    quanteda:::qatd_cpp_set_load_factor("pattern", -1.0)
    quanteda:::qatd_cpp_set_load_factor("pattern", 2.0)
    quanteda:::qatd_cpp_set_load_factor("ngrams", 0.7)
    quanteda:::qatd_cpp_set_load_factor("ngrams", -1.0)
    quanteda:::qatd_cpp_set_load_factor("ngrams", 2.0)
    expect_equal(quanteda:::qatd_cpp_get_load_factor(),
                 list(pattern = 0.2, ngrams = 0.7), tolerance = 0.001)
    
})

test_that("check_class works", {
    expect_silent(
        quanteda:::check_class("dfm", "dfm_select")
    )
    expect_error(
        quanteda:::check_class("character", "dfm_select"), 
        "dfm_select() only works on dfm objects.", fixed = TRUE
    )
    expect_error(
        quanteda:::check_class("dfm", "tokens"), 
        "tokens() only works on character, corpus, list, tokens objects.", fixed = TRUE
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

