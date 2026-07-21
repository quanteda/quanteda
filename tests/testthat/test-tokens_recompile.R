
test_that("raise error when tokens are invalid", {

    toks <- quanteda:::build_tokens(
        list(1:5),
        c('a', 'b', 'c'),
        docvars = quanteda:::make_docvars(1L)
    )
    
    expect_error(tokens_recompile(toks, force = TRUE),
                 "Invalid tokens object")
})

test_that("empty tokens become paddings", {
    
    toks <- quanteda:::build_tokens(
        list(1:5),
        c('a', 'b', 'c', '', 'e'),
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks_re <- tokens_recompile(toks, force = TRUE)
    expect_true(attr(toks_re, "padding"))
    expect_equal(attr(toks_re, "types"),
                 c('a', 'b', 'c', 'e'))
})

test_that("padding is detected", {
    
    toks <- quanteda:::build_tokens(
        list(0:26), # has padding, but no gap
        letters,
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks_re <- quanteda:::tokens_recompile(toks, force = TRUE)
    expect_true(attr(toks_re, 'padding'))
    
})

test_that("non-ascii types are UTF8 encoded", {
    
    toks <- quanteda:::build_tokens(
        list(c(1, 2, 3)),
        c('あ', 'い', 'う', 'え', 'お'),
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks_re <- quanteda:::tokens_recompile(toks, force = TRUE)
    expect_equal(
        Encoding(attr(toks_re, 'types')), 
        rep('UTF-8', 3)
    )
})

test_that("keep gap and dupli argument works, #1278", {
    
    toks <- quanteda:::build_tokens(
        list(c(2, 3, 4)),
        c('a', 'b', 'c', 'c', 'd'),
        docvars = quanteda:::make_docvars(1L)
    )

    toks_re <- quanteda:::tokens_recompile(toks, force = TRUE)
    expect_equal(attr(toks_re, 'padding'), TRUE)
    expect_equal(attr(toks_re, 'types'), c("b", "c"))
    
    toks2 <- quanteda:::build_tokens(
        list(c(0, 2, 3, 4)),
        c('a', 'b', 'c', 'c', 'd'),
        padding = TRUE,
        docvars = quanteda:::make_docvars(1L)
    )
    
    toks_re2 <- quanteda:::tokens_recompile(toks2, force = TRUE)
    expect_equal(attr(toks_re2, 'padding'), TRUE)
    expect_equal(attr(toks_re2, 'types'), c("b", "c"))
    
    toks_err <- quanteda:::build_tokens(
        list(c(2, 3, 4, 6)),
        c('a', 'b', 'c', 'c', 'd'),
        padding = TRUE,
        docvars = quanteda:::make_docvars(1L)
    )
    expect_error(
        quanteda:::tokens_recompile(toks_err, force = TRUE),
        "Invalid tokens object"
    )
    
})

test_that("set encoding when no gap or duplication is found, #1387", {
    
    toks <- tokens("привет tschüß bye")
    toks <- quanteda:::tokens_recompile(toks)
    expect_equal(Encoding(types(toks)), 
                 c("UTF-8", "UTF-8", "unknown")) 
})

