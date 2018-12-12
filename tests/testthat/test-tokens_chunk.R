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

test_that("tokens_chunk() works with sizes longer than tokens length", {
    toks <- tokens(c(d1 = "a b c d e", d2 = "a b c"))
    expect_identical(
        as.list(tokens_chunk(toks, size = 4, discard_remainder = FALSE)),
        list(d1.1 = c("a", "b", "c", "d"),
             d1.2 = "e",
             d2.1 = c("a", "b", "c"))
    )
    expect_identical(
        as.list(tokens_chunk(toks, size = 4, discard_remainder = TRUE)),
        list(d1 = c("a", "b", "c", "d"),
             d2 = character(0))
    )
})


context("compare with another method")

#' Segment tokens object by chunks of a given size (alternative)
#' 
#' Segment tokens by splitting into equally sized chunks.
#' @param x \link{tokens} object whose token elements will be segmented into
#'   chunks
#' @param size integer; the size of the chunks in tokens
#' @param discard_remainder logical; if \code{TRUE}, discard any ending chunk
#'   whose length is less than \code{size}
#' @return A \link{tokens} object whose documents have been split into chunks,
#'   similar to \code{\link{tokens_segment}}.
#' @keywords tokens internal
#' @examples 
#' txts <- c(doc1 = "Fellow citizens, I am again called upon by the voice of 
#'                   my country to execute the functions of its Chief Magistrate.",
#'           doc2 = "When the occasion proper for it shall arrive, I shall 
#'                   endeavor to express the high sense I entertain of this
#'                   distinguished honor.")
#' toks <- tokens(txts)
#' tokens_chunk(toks, size = 5)
tokens_chunk2 <- function(x, size, discard_remainder = TRUE) {
    UseMethod("tokens_chunk")
}

#' @rdname tokens_chunk2
tokens_chunk2.tokens <- function(x, size, discard_remainder = TRUE) {
    attrs <- attributes(x)

    # choose something very unlikely to be found
    septoken <- "\u058E"
    # just to be sure
    if (septoken %in% types(x)) stop("need a different septoken!")

    septoken_index <- max(unlist(unclass(x))) + 1
    attrs$types <- c(types(x), septoken)

    x <- lapply(unclass(x), insert_values,
                n = size, val = septoken_index, truncate = discard_remainder)
    attributes(x) <- attrs

    # segment on the inserted pattern
    ret <- tokens_segment(x, pattern = septoken, extract_pattern = TRUE)
    # return without the extracted pattern in the docvars
    attr(ret, "docvars")$pattern <- NULL
    ret
}

#' Insert integers into a vector at defined intervals
#' 
#' Inserts \code{val} into a numeric vector \code{x} every \code{n} values.
#' @param x a numeric vector
#' @param n the length of the partitions of \code{x} to be divided by \code{val}
#' @param val the numeric value to be inserted
#' @param truncate logical; if \code{TRUE}, discard any remaining sequence less
#'   than \code{n} at the end of the chunked tokens.
#' max(unlist(unclass(x)))
insert_values <- function(x, n, val, truncate = TRUE) {
    ret <- unlist(Map(c, split(x, (seq_along(x) - 1) %/% n), list(val)), use.names = FALSE)
    # trim the value that is always added to the end
    ret <- head(ret, -1)
    # truncate any short part at the end plus the last val
    if (truncate && length(x) > n && (shortend <- length(x) %% n)) {
        ret <- head(ret, -(shortend + 1))
    }
    ret
}

test_that("tokens_chunk and tokens_chunk2 are the same", {
    toks <- tokens(data_corpus_inaugural)
    expect_identical(
        tokens_chunk(toks, size = 100, discard_remainder = FALSE),
        tokens_chunk2(toks, size = 100, discard_remainder = FALSE)
    )
    expect_identical(
        tokens_chunk(toks, size = 100, discard_remainder = TRUE),
        tokens_chunk2(toks, size = 100, discard_remainder = TRUE)
    )
})

test_that("insert_values works", {
    expect_identical(insert_values(1, 3, 99), 1)
    expect_identical(
        insert_values(1:3, 3, 99, truncate = TRUE),
        c(1, 2, 3)
    )
    expect_identical(insert_values(1:5, 3, 99, truncate = TRUE), c(1, 2, 3))
    expect_identical(
        quanteda:::insert_values(1:5, 3, 99, truncate = FALSE),
        c(1:3, 99, 4:5)
    )
    expect_identical(insert_values(1:6, 3, 99), c(1:3, 99, 4:6))
})
