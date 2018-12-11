context("tokens_chunk works")

test_that("tokens_chunk works", {
    txt <- c(d1 = "a b c d", d2 = "e f g")

    corp <- corpus(txt, docvars = data.frame(title = c("title1", "title2"),
                                             stringsAsFactors = FALSE))
    toks <- tokens(corp)

    toks_chunk1 <- tokens_chunk(toks, size = 5, discard_remainder = FALSE)
    expect_identical(as.list(toks_chunk1),
                     list(d1 = c("a", "b", "c", "d"),
                          d2 = c("e", "f", "g")))
    expect_identical(attr(toks_chunk1, "docvars"),
                     data.frame("title" = c("title1", "title2"),
                                "_document" = c("d1", "d2"),
                                "_docid" = c(1L, 2L),
                                "_segid" = c(1L, 1L),
                                row.names = c("d1", "d2"),
                                stringsAsFactors = FALSE,
                                check.names = FALSE))

    toks_chunk2 <- tokens_chunk(toks, size = 2, discard_remainder = FALSE)
    expect_identical(as.list(toks_chunk2),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("c", "d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("g")))
    expect_identical(attr(toks_chunk2, "docvars"),
                     data.frame("title" = c("title1", "title1", "title2", "title2"),
                                "_document" = c("d1", "d1", "d2", "d2"),
                                "_docid" = c(1L, 1L, 2L, 2L),
                                "_segid" = c(1L, 2L, 1L, 2L),
                                row.names = c("d1.1", "d1.2", "d2.1", "d2.2"),
                                stringsAsFactors = FALSE,
                                check.names = FALSE))

    toks_chunk3 <- tokens_chunk(toks, 2, overlap = TRUE, discard_remainder = FALSE)
    expect_identical(as.list(toks_chunk3),
                     list(d1.1 = c("a", "b"),
                          d1.2 = c("b", "c"),
                          d1.3 = c("c", "d"),
                          d1.4 = c("d"),
                          d2.1 = c("e", "f"),
                          d2.2 = c("f", "g"),
                          d2.3 = c("g")))
    expect_identical(attr(toks_chunk3, "docvars"),
                     data.frame("title" = c("title1", "title1", "title1", "title1", "title2",
                                            "title2", "title2"),
                                "_document" = c("d1", "d1", "d1", "d1", "d2", "d2", "d2"),
                                "_docid" = c(1L, 1L, 1L, 1L, 2L, 2L, 2L),
                                "_segid" = c(1L, 2L, 3L, 4L, 1L, 2L, 3L),
                                row.names = c("d1.1", "d1.2", "d1.3", "d1.4", "d2.1", "d2.2",
                                              "d2.3"),
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

test_that("tokens_chunk works", {
    toks <- tokens(c("a b c d e f", "a a b d c"))
    expect_is(tokens_chunk(toks, size = 3), "tokens")
    expect_equivalent(
        as.list(tokens_chunk(toks, 3, discard_remainder = TRUE)),
        list(c("a", "b", "c"), c("d", "e", "f"), c("a", "a", "b"))
    )
    expect_equivalent(
        as.list(tokens_chunk(toks, 3, discard_remainder = FALSE)),
        list(c("a", "b", "c"), c("d", "e", "f"), c("a", "a", "b"), c("d", "c"))
    )
    expect_identical(
        metadoc(tokens_chunk(toks, 3)),
        data.frame("_document" = paste0("text", c(1, 1, 2)),
                   "_docid" = c(1L, 1L, 2L),
                   "_segid" = c(1L, 2L, 1L),
                   stringsAsFactors = FALSE,
                   row.names = c("text1.1", "text1.2", "text2.1"),
                   check.names = FALSE)
    )
})

context("Ken's tokens_chunk() comparisons - can remove later")

test_that("tokens_chunk and tokens_chunk2 are the same", {
    toks <- tokens(data_corpus_inaugural)
    expect_identical(
        tokens_chunk(toks, size = 100, discard_remainder = FALSE),
        quanteda:::tokens_chunk2(toks, size = 100, discard_remainder = FALSE)
    )
    expect_identical(
        tokens_chunk(toks, size = 100, discard_remainder = TRUE),
        quanteda:::tokens_chunk2(toks, size = 100, discard_remainder = TRUE)
    )
})

test_that("insert_values works", {
    expect_identical(quanteda:::insert_values(1, 3, 99), 1)
    expect_identical(
        quanteda:::insert_values(1:3, 3, 99, truncate = TRUE),
        c(1, 2, 3)
    )
    expect_identical(quanteda:::insert_values(1:5, 3, 99, truncate = TRUE), c(1, 2, 3))
    expect_identical(
        quanteda:::insert_values(1:5, 3, 99, truncate = FALSE),
        c(1:3, 99, 4:5)
    )
    expect_identical(quanteda:::insert_values(1:6, 3, 99), c(1:3, 99, 4:6))
})
