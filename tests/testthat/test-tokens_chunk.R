context("test tokens_chunk")

test_that("tokens_chunk works", {
    txt <- c(d1 = "a b c d", d2 = "e f g")

    corp <- corpus(txt, docvars = data.frame(title = c("title1", "title2"),
                                             stringsAsFactors = FALSE))
    toks <- tokens(corp)

    toks_chunk1 <- tokens_chunk(toks, size = 5)
    expect_identical(as.list(toks_chunk1),
                     list(d1 = c("a", "b", "c", "d"),
                          d2 = c("e", "f", "g")))
    expect_identical(attr(toks_chunk1, "docvars"),
                     data.frame("docname_" = c("d1", "d2"),
                                "docid_" = factor(c("d1", "d2")),
                                "segid_" = c(1L, 1L),
                                "title" = c("title1", "title2"),
                                stringsAsFactors = FALSE))

    toks_chunk2 <- tokens_chunk(toks, size = 2)
    expect_identical(as.list(toks_chunk2),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("c", "d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("g")))
    expect_identical(attr(toks_chunk2, "docvars"),
                     data.frame("docname_" = c("d1.1", "d1.2", "d2.1", "d2.2"),
                                "docid_" = factor(c("d1", "d1", "d2", "d2")),
                                "segid_" = c(1L, 2L, 1L, 2L),
                                "title" = c("title1", "title1", "title2", "title2"),
                                stringsAsFactors = FALSE,
                                check.names = FALSE))
    
    toks_chunk3 <- tokens_chunk(toks, size = 2, use_docvars = FALSE)
    expect_identical(as.list(toks_chunk3),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("c", "d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("g")))
    expect_identical(attr(toks_chunk3, "docvars"),
                     data.frame("docname_" = c("d1.1", "d1.2", "d2.1", "d2.2"),
                                "docid_" = factor(c("d1", "d1", "d2", "d2")),
                                "segid_" = c(1L, 2L, 1L, 2L),
                                stringsAsFactors = FALSE,
                                check.names = FALSE))

    toks_chunk4 <- tokens_chunk(toks, 2, overlap = 1)
    expect_identical(as.list(toks_chunk4),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("b", "c"),
                          d1.3 = c("c", "d"),
                          d1.4 = c("d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("f", "g"),
                          d2.3 = c("g")))
    expect_identical(attr(toks_chunk4, "docvars"),
                     data.frame("docname_" = c("d1.1", "d1.2", "d1.3", "d1.4", "d2.1", "d2.2", "d2.3"),
                                "docid_" = factor(c("d1", "d1", "d1", "d1", "d2", "d2", "d2")),
                                "segid_" = c(1L, 2L, 3L, 4L, 1L, 2L, 3L),
                                "title" = c("title1", "title1", "title1", "title1", "title2",
                                            "title2", "title2"),
                                stringsAsFactors = FALSE,
                                check.names = FALSE))
    
})

test_that("tokens_chunk raises error for invalid size", {
    toks <- tokens(c(d1 = "a b c d", d2 = "e f g"))
    expect_error(tokens_chunk(toks, -1),
                 "Chunk size must be larger than or equal to 1")
    expect_error(tokens_chunk(toks, 0),
                 "Chunk size must be larger than or equal to 1")
    expect_error(tokens_chunk(toks, c(1, 3)),
                 "Size must be a single integer")
})

test_that("tokens_chunk raises error for invalid overlap", {
    toks <- tokens(c(d1 = "a b c d", d2 = "e f g"))
    expect_error(tokens_chunk(toks, 2, 2),
                 "Overlap must be smaller than size")
    expect_error(tokens_chunk(toks, 2, -1),
                 "Overlap must be a positive value")
})

test_that("tokens_chunk works", {
    toks <- tokens(c("a b c d e f", "a a b d c"))
    expect_is(tokens_chunk(toks, size = 3), "tokens")
    expect_equivalent(
        as.list(tokens_chunk(toks, 3)),
        list(c("a", "b", "c"), c("d", "e", "f"), c("a", "a", "b"), c("d", "c"))
    )
    expect_identical(
        attr(tokens_chunk(toks, 3), "docvars"),
        data.frame("docname_" = c("text1.1", "text1.2", "text2.1", "text2.2"),
                   "docid_" = factor(c("text1", "text1", "text2", "text2")),
                   "segid_" = c(1L, 2L, 1L, 2L),
                   stringsAsFactors = FALSE,
                   check.names = FALSE)
    )
})

test_that("tokens_chunk() works with sizes longer than tokens length", {
    toks <- tokens(c(d1 = "a b c d e", d2 = "a b c"))
    expect_identical(
        as.list(tokens_chunk(toks, size = 4)),
        list(d1.1 = c("a", "b", "c", "d"),
             d1.2 = "e",
             d2.1 = c("a", "b", "c"))
    )
})
