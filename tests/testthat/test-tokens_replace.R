context("test tokens_replace")


test_that("test tokens_replace", {
    
    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    toks <- tokens(txt)
    
    # case-insensitive
    expect_equal(as.list(tokens_replace(toks, c('aa', 'bb'), c('a', 'b'), case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "b", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # case-sensitive
    expect_equal(as.list(tokens_replace(toks, c('aa', 'bb'), c('a', 'b'), case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # duplicated types in from
    expect_equal(as.list(tokens_replace(toks, c('aa', 'aa'), c('a', 'aaa'), case_insensitive = FALSE)),
                 list(doc1 = c("a", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "bb", "cc", "DD", "ee")))
    
    # equivalent to tokens conversion method
    type <- types(toks)
    expect_equal(tokens_replace(toks, type, char_toupper(type), case_insensitive = FALSE),
                 tokens_toupper(toks))
    
    # error when lenfths of from and to are different
    expect_error(tokens_replace(toks, c('aa', 'bb'), c('a')),
                 "Lengths of 'from' and 'to' must be the same")
    
    # does nothing when input vector is zero length
    expect_equal(tokens_replace(toks, c(), c()),
                 toks)
    
})
