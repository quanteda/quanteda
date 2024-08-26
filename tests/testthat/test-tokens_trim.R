test_that("tokens_trim works", {
    
    toks <- tokens(c(d1 = "a b c d e", 
                     d2 = "a a b b e f", 
                     d3 = "b c e e f f f"))
    s <- sum(ntoken(toks))
    
    expect_equal(length(types(tokens_trim(toks, min_termfreq = 0.5, termfreq_type = "quantile"))), 4)
    expect_equal(length(types(tokens_trim(toks, min_termfreq = 3 / s, termfreq_type = "prop"))), 4)
    expect_equal(length(types(tokens_trim(toks, min_termfreq = 3))), 4)
    
    expect_equal(length(types(tokens_trim(toks, max_termfreq = 0.8, termfreq_type = "quantile"))), 6)
    expect_equal(length(types(tokens_trim(toks, max_termfreq = 4 / s, termfreq_type = "prop"))), 6)
    expect_equal(length(types(tokens_trim(toks, max_termfreq = 4))), 6)
    
    expect_equal(length(types(tokens_trim(toks, min_docfreq = 0.5, docfreq_type = "quantile"))), 5)
    expect_equal(length(types(tokens_trim(toks, min_docfreq = 2 / 3, docfreq_type = "prop"))), 5)
    expect_equal(length(types(tokens_trim(toks, min_docfreq = 2))), 5)
    
    expect_equal(length(types(tokens_trim(toks, max_docfreq = 0.5, docfreq_type = "quantile"))), 4)
    expect_equal(length(types(tokens_trim(toks, max_docfreq = 2 / 3, docfreq_type = "prop"))), 4)
    expect_equal(length(types(tokens_trim(toks, max_docfreq = 2))), 4)
    
    expect_equal(length(types(tokens_trim(toks, min_termfreq = 2, termfreq_type = "rank"))), 3)
    expect_equal(length(types(tokens_trim(toks, min_termfreq = 4))), 3)
    
    expect_equal(length(types(tokens_trim(toks, max_termfreq = 4, termfreq_type = "rank"))), 3)
    expect_equal(length(types(tokens_trim(toks, max_termfreq = 3))), 3)
    
    expect_equal(length(types(tokens_trim(toks, min_docfreq = 1, docfreq_type = "rank"))), 2)
    expect_equal(length(types(tokens_trim(toks, min_docfreq = 3))), 2)
    
    expect_equal(length(types(tokens_trim(toks, max_docfreq = 2, docfreq_type = "rank"))), 4)
    expect_equal(length(types(tokens_trim(toks, max_docfreq = 2))), 4)
    
})

test_that("tokens_trim works with padding", {
    
    txt <- c(d1 = "a b c d e ?", 
             d2 = "a ! a b b e f", 
             d3 = "b c e e . f f f")
    toks <- tokens(txt, remove_punct = TRUE, padding = TRUE)
    
    expect_equal(
        types(tokens_trim(toks, min_termfreq = 4)), 
        c("b", "e", "f")
    )
    
    expect_equal(
        types(tokens_trim(toks, min_termfreq = 4, padding = TRUE)), 
        c("b", "e", "f")
    )
    
    expect_equal(
        types(tokens_trim(toks, min_docfreq = 3)), 
        c("b", "e")
    )
    
    expect_equal(
        types(tokens_trim(toks, min_docfreq = 3, padding = TRUE)), 
        c("b", "e")
    )
    

    expect_equal(
        types(tokens_trim(toks, max_termfreq = 2)), 
        c("c", "d")
    )
    
    expect_equal(
        types(tokens_trim(toks, max_termfreq = 2, padding = TRUE)), 
        c("c", "d")
    )
    
    expect_equal(
        types(tokens_trim(toks, max_docfreq = 1)), 
        c("d")
    )

    expect_equal(
        types(tokens_trim(toks, max_docfreq = 1, padding = TRUE)), 
        c("d")
    )
})
