test_that("tokens_recompile: tokens_tolower", {
    toks1 <- tokens(c(one = "a b c d A B C D",
                      two = "A B C d"))
    attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
    expect_equal(
        attr(quanteda:::tokens_recompile(toks1), "types"),
        letters[1:4]
    )
    expect_equal(
        unique(unlist(unclass(quanteda:::tokens_recompile(toks1)))),
        1:4
    )
    expect_equal(
        quanteda:::tokens_recompile(toks1, method = "C++"),
        quanteda:::tokens_recompile(toks1, method = "R")
    )
})

test_that("tokens_recompile: tokens_wordstem", {
    toks <- tokens(c(one = "stems stemming stemmed"))
    attr(toks, "types") <- char_wordstem(attr(toks, "types"))
    expect_equal(
        attr(quanteda:::tokens_recompile(toks), "types"),
        "stem"
    )
    expect_equal(
        unique(unlist(unclass(quanteda:::tokens_recompile(toks)))),
        1
    )
    expect_equal(
        quanteda:::tokens_recompile(toks, method = "C++"),
        quanteda:::tokens_recompile(toks, method = "R")
    )
    expect_equal(
        as.character(tokens_wordstem(toks)),
        rep("stem", 3)
    )
})


test_that("tokens_recompile: tokens_select w/gaps", {
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


test_that("tokens_recompile: preserves encoding", {
    
    txt <- c(French = "Pêcheur pêcheur Français")
    Encoding(txt) <- "UTF-8"
    toks <- tokens(txt)
    attr(toks, "types") <- char_tolower(attr(toks, "types"))
    
    expect_equal(
        Encoding(as.character(quanteda:::tokens_recompile(toks, method = "R"), "types")),
        rep("UTF-8", 3)
    )
    expect_equal(
        Encoding(as.character(quanteda:::tokens_recompile(toks, method = "C++"), "types")),
        rep("UTF-8", 3)
    )
})

test_that("tokens_recompile: [ works for tokens", {
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

test_that("tokens_recompile: selecting all tokens to produce and empty document", {
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

test_that("corrupt tokens object does not crash R", {
    skip_on_os("solaris")
    
    toks <- list(1:10)
    attr(toks, 'types') <- c('a', 'b', 'c') # Shorter than 10
    attr(toks, 'class') <- 'tokens'
    expect_error(quanteda:::tokens_recompile(toks, 'C++'))
})

test_that("tokens_recompile: flag use of padding even when it does not reindex tokens", {
    
    toks <- quanteda:::build_tokens(
        list(0:26), # has padding, but no gap
        letters,
        docvars = quanteda:::make_docvars(1L)
    )
    expect_true(attr(quanteda:::tokens_recompile(toks, 'C++'), 'padding'))
    
})

test_that("non-ascii types are UTF8 encoded", {
    
    toks <- quanteda:::build_tokens(
        list(c(1, 2, 3)),
        c('あ', 'い', 'う', 'え', 'お'),
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks2 <- quanteda:::tokens_recompile(toks, 'C++')
    expect_equal(Encoding(attr(toks2, 'types')), rep('UTF-8', 3))
})

test_that("keep gap and dupli argument works, #1278", {
    
    toks <- quanteda:::build_tokens(
        list(c(2, 3, 4)),
        c('a', 'b', 'c', 'c', 'd'),
        docvars = quanteda:::make_docvars(1L)
    )

    toks2 <- quanteda:::tokens_recompile(toks, 'C++')
    expect_equal(attr(toks2, 'padding'), TRUE)
    expect_equal(attr(toks2, 'types'), c("b", "c"))
    
    expect_equal(quanteda:::tokens_recompile(toks, 'C++'),
                 quanteda:::tokens_recompile(toks, 'R'))
    
    toks_pad <- quanteda:::build_tokens(
        list(c(0, 2, 3, 4)),
        c('a', 'b', 'c', 'c', 'd'),
        padding = TRUE,
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks_pad2 <- quanteda:::tokens_recompile(toks_pad, 'C++')
    expect_equal(attr(toks_pad2, 'padding'), TRUE)
    expect_equal(attr(toks_pad2, 'types'), c("b", "c"))
    
    expect_equal(quanteda:::tokens_recompile(toks_pad, 'C++'),
                 quanteda:::tokens_recompile(toks_pad, 'R'))
    
    toks_err <- quanteda:::build_tokens(
        list(c(2, 3, 4, 6)),
        c('a', 'b', 'c', 'c', 'd'),
        padding = TRUE,
        docvars = quanteda:::make_docvars(1L)
    )
    expect_error(
        quanteda:::tokens_recompile(toks_err, 'C++')
    )
    
})

test_that("set encoding when no gap or duplication is found, #1387", {
    
    toks <- tokens("привет tschüß bye")
    toks <- quanteda:::tokens_recompile(toks)
    expect_equal(Encoding(types(toks)), 
                 c("UTF-8", "UTF-8", "unknown")) 
})

