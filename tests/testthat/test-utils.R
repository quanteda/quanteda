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

test_that("friendly_class_undefined_message", {
    expect_error(
        as.tokens(data_dfm_lbgexample),
        "as.tokens\\(\\) only works on.*list.*spacyr_parsed.*objects"
    )
})


test_that("pattern2id is working with collocations", {
    
    txt <- c(". . . . a b c . . a b c . . . c d e",
             "a b . . a b . . a b . . a b . a b",
             "b c d . . b c . b c . . . b c")
    toks <- tokens(txt)
    type <- types(toks)
    col <- textstat_collocations(toks, size = 2:3)
    ids <- quanteda:::pattern2id(col, type, 'fixed', TRUE)
    expect_equal(col$collocation, vapply(ids, function(x, y) paste0(y[x], collapse = ' '), character(1), type))
    
})

test_that("pattern2id is working with a list", {
    
    type <- letters
    pat <- c('a b', 'c d', 'e f g')
    ids <- quanteda:::pattern2id(phrase(pat), type, 'fixed', TRUE)
    expect_equal(pat, vapply(ids, function(x, y) paste0(y[x], collapse = ' '), character(1), type))
    
})

test_that("pattern2id is working with empty patterns", {
    
    col <- data.frame()
    class(col) <- c('collocations', 'data.frame')
    pat <- list()
    expect_silent(quanteda:::pattern2id(col, types(toks), 'fixed', TRUE))
    expect_silent(quanteda:::pattern2id(pat, types(toks), 'fixed', TRUE))
    
})
