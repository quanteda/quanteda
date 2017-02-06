context("testing tokens_hashed_recompile")

test_that("tokens_hashed_recompile: tokens_tolower", {
    toks1 <- tokens(c(one = "a b c d A B C D",
                      two = "A B C d"))
    attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
    expect_equal(
        attr(quanteda:::tokens_hashed_recompile(toks1), "types"),
        letters[1:4]
    )
    expect_equal(
        unique(unlist(unclass(quanteda:::tokens_hashed_recompile(toks1)))),
        1:4
    )
    expect_equal(
        quanteda:::tokens_hashed_recompile(toks1, method = "C++"),
        quanteda:::tokens_hashed_recompile(toks1, method = "R")
    )
})

test_that("tokens_hashed_recompile: tokens_wordstem", {
    toks <- tokens(c(one = "stems stemming stemmed"))
    attr(toks, "types") <- char_wordstem(attr(toks, "types"))
    expect_equal(
        attr(quanteda:::tokens_hashed_recompile(toks), "types"),
        "stem"
    )
    expect_equal(
        unique(unlist(unclass(quanteda:::tokens_hashed_recompile(toks)))),
        1
    )
    expect_equal(
        quanteda:::tokens_hashed_recompile(toks, method = "C++"),
        quanteda:::tokens_hashed_recompile(toks, method = "R")
    )
    expect_equal(
        as.character(tokens_wordstem(toks)),
        rep("stem", 3)
    )
})


test_that("tokens_hashed_recompile: tokens_select w/gaps", {
    toks1 <- tokens(c(one = "a b c d A B C D",
                      two = "A B C d"))
    expect_equal(
        unique(unlist(unclass(tokens_select(toks1, c("b", "d"))))),
        1:4
    )
    expect_equal(
        unique(unlist(unclass(tokens_select(toks1, c("b", "d"), padding = TRUE)))),
        0:4
    )
    expect_equal(
        attr(tokens_select(toks1, c("b", "d")), "types"),
        c("b", "d", "B", "D")
    )
})


test_that("tokens_hashed_recompile: preserves encoding", {
    skip_on_appveyor()  
    txt <- c(French = "Pêcheur pêcheur Français")
    Encoding(txt) <- "UTF-8"
    toks <- tokens(txt)
    attr(toks, "types") <- char_tolower(attr(toks, "types"))
    
    expect_equal(
        Encoding(as.character(quanteda:::tokens_hashed_recompile(toks, method = "R"), "types")),
        rep("UTF-8", 3)
    )
    expect_equal(
        Encoding(as.character(quanteda:::tokens_hashed_recompile(toks, method = "C++"), "types")),
        rep("UTF-8", 3)
    )
})

test_that("tokens_hashed_recompile: ngrams", {
    toks <- tokens(c(one = "a b c"))

    expect_equal(
        as.list(tokens_ngrams(toks, 2:3)),
        list(one = c("a_b", "b_c", "a_b_c"))
    )

    expect_equal(
        attributes(tokens_ngrams(toks, 2:3, concatenator = " "))$concatenator,
        " "
    )

    expect_equal(
        attributes(tokens_ngrams(toks, 2:3, concatenator = " "))$ngrams,
        2L:3L
    )

    attr(toks, "types") <- char_ngrams(attr(toks, "types"), 2:3)
    expect_equal(
        quanteda:::tokens_hashed_recompile(toks, method = "R"),
        quanteda:::tokens_hashed_recompile(toks, method = "C++")
    )
})

test_that("tokens_hashed_recompile: [ works for tokens", {
    toks <- tokens(c(one = "a b c d",
                     two = "x y z",
                     three = "e f g h i j k"))
    expect_equal(
        unclass(toks[2])[[1]], 
        1:3
    )
    expect_equal(
        attr(toks[1], "types"), 
        letters[1:4]
    )
})

test_that("tokens_hashed_recompile: selecting all tokens to produce and empty document", {
    toks <- tokens(c(one = "a b c d",
                     two = "x y z"))
    toks <- tokens_select(toks, letters[1:4])

    expect_equal(
        attr(toks, "types"), 
        letters[1:4]
    )
    expect_equal(
        unclass(toks)[2], 
        list(two = integer(0))
    )
    expect_equal(
        as.list(toks[2]), 
        list(two = character(0))
    )
    
})

test_that("tokens_hashed_recompile: corrupt tokens object does not crash R", {
    
    toks <- list(1:10)
    attr(toks, 'types') <- c('a', 'b', 'c') # Shorter than 10
    attr(toks, 'class') <- 'tokens'
    expect_error(quanteda:::tokens_hashed_recompile(toks, 'C++'))
    
})

test_that("tokens_hashed_recompile: flag use of padding even when it does not reindex tokens", {
    
    toks <- list(0:26) # has padding, but no gap
    attr(toks, 'types') <- letters # no duplication
    attr(toks, 'class') <- 'tokens'
    expect_true(attr(quanteda:::tokens_hashed_recompile(toks, 'C++'), 'padding'))
    
})


