context("tokens_chunk works")

test_that("tokens_chunk works", {
    txt <- c(d1 = "a b c d", d2 = "e f g")
    
    corp <- corpus(txt, docvars = data.frame(title = c("title1", "title2")))
    toks <- tokens(corp)
    
    toks_chunk1 <- tokens_chunk(toks, 5)
    expect_identical(as.list(toks_chunk1),
                     list(d1 = c("a", "b", "c", "d"),
                          d2 = c("e", "f", "g")))
    expect_identical(docvars(toks_chunk1),
                     data.frame(title = c("title1", "title2"),
                                row.names = c("d1", "d2")))
    
    toks_chunk2 <- tokens_chunk(toks, 2)
    expect_identical(as.list(toks_chunk2),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("c", "d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("g")))
    expect_identical(docvars(toks_chunk2),
                     data.frame(title = c("title1", "title1", "title2", "title2"),
                                row.names = c("d1.1", "d1.2", "d2.1", "d2.2")))
    
    toks_chunk3 <- tokens_chunk(toks, 2, overlap = TRUE)
    expect_identical(as.list(toks_chunk3),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("b", "c"),
                          d1.3 = c("c", "d"),
                          d1.4 = c("d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("f", "g"),
                          d2.3 = c("g")))
    expect_identical(docvars(toks_chunk3),
                     data.frame(title = c("title1", "title1", "title1", "title1", "title2", "title2", "title2"),
                                row.names = c("d1.1", "d1.2", "d1.3", "d1.4", "d2.1", "d2.2", "d2.3")))
    
})

test_that("tokens_chunk raies error for invalide size", {
    toks <- tokens(c(d1 = "a b c d", d2 = "e f g"))
    expect_error(tokens_chunk(toks, -1),
                 "Chunk size must be larger than or equal to 1")
    expect_error(tokens_chunk(toks, 0),
                 "Chunk size must be larger than or equal to 1")
    expect_error(tokens_chunk(toks, c(1, 3)),
                 "Size must be a single integer")
    
    
})
