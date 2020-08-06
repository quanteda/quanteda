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

test_that("pattern2list is working with collocations", {
    txt <- c(". . . . a b c . . a b c . . . c d e",
             "a b . . a b . . a b . . a b . a b",
             "b c d . . b c . b c . . . b c")
    toks <- tokens(txt)
    type <- types(toks)
    col <- textstat_collocations(toks, size = 2:3)
    ids <- quanteda:::pattern2list(col, type, 'fixed', TRUE)
    expect_equivalent(col$collocation, 
                      vapply(ids, function(x, y) paste0(y[x], collapse = " "), character(1), type))
    expect_equal(names(ids), col$collocation)
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

test_that("check_font is working", {
    # skip_on_os("windows")
    # skip_on_os("mac")
    skip_on_cran()
    expect_error(quanteda:::check_font("XXXXX"), "XXXXX is not found on your system") 
    # expect_equal(quanteda:::check_font("Ubuntu"), "Ubuntu")
    expect_equal(quanteda:::check_font("sans"), "sans")
    expect_equal(quanteda:::check_font("serif"), "serif")
    expect_equal(quanteda:::check_font("mono"), "mono")
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

test_that("sample_bygroup works with sizes", {
    grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
    
    # sampling same size each group
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, size = 2, replace = TRUE)
    expect_true(all(tmp[1:2] >= 0 & tmp[1:2] <= 3))
    expect_true(all(tmp[3:4] >= 4 & tmp[3:4] <= 7))
    expect_true(all(tmp[5:6] >= 8 & tmp[5:6] <= 10))
    
    # sampling from a vector of sizes
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, size = c(1, 1, 3), replace = TRUE)
    expect_true(all(tmp[1] >= 0 & tmp[1] <= 3))
    expect_true(all(tmp[2] >= 4 & tmp[2] <= 7))
    expect_true(all(tmp[3:5] >= 8 & tmp[3:5] <= 10))
    
    # default group size sampling
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, replace = TRUE)
    expect_true(all(tmp[1:3] >= 0 & tmp[1:3] <= 3))
    expect_true(all(tmp[4:7] >= 4 & tmp[4:7] <= 7))
    expect_true(all(tmp[8:10] >= 8 & tmp[8:10] <= 10))
    
    # oversampling within group
    tmp <- quanteda:::sample_bygroup(c(1:2, c(101:102)), 
                                     group = rep(letters[1:2], each = 2),
                                     size = 5, replace = TRUE)
    expect_true(all(tmp[1:5] >= 0 & tmp[1:5] <= 2))
    expect_true(all(tmp[6:10] >= 100 & tmp[6:10] <= 102))
    
    expect_error(
        quanteda:::sample_bygroup(1:10, group = grvec[1:6], size = c(2, 3), replace = TRUE),
        "group not equal in length of x"
    )
    expect_error(
        quanteda:::sample_bygroup(1:10, group = grvec, size = c(2, 3), replace = TRUE),
        "size not equal in length to the number of groups"
    )
})

test_that("sample_bygroup works with groups of size 1", {
    grvec <- c(rep("a", 3), rep("b", 1))
    for (i in seq_len(10)) {
        tmp <- quanteda:::sample_bygroup(1:4, group = grvec, replace = TRUE)
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
