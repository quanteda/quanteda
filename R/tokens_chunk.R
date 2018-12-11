#' Segment tokens object by chunks of a given size
#' 
#' Segment tokens by splitting into equally sized chunks.
#' @param x \link{tokens} object whose token elements will be segmented into
#'   chunks
#' @param size integer; the size of the chunks in tokens
#' @param overlap logical; if \code{TRUE}, the last token of a chunk will be the
#'   first token of the next chunk
#' @param discard_remainder logical; if \code{TRUE}, discard any ending chunk
#'   whose length is less than \code{size}
#' @return A \link{tokens} object whose documents have been split into chunks of
#'   length \code{size}, similar to \code{\link{tokens_segment}}.
#' @keywords tokens
#' @export
#' @examples 
#' txts <- c(doc1 = "Fellow citizens, I am again called upon by the voice of 
#'                   my country to execute the functions of its Chief Magistrate.",
#'           doc2 = "When the occasion proper for it shall arrive, I shall 
#'                   endeavor to express the high sense I entertain of this
#'                   distinguished honor.")
#' toks <- tokens(txts)
#' tokens_chunk(toks, size = 5)
#' tokens_chunk(toks, size = 5, overlap = TRUE)
#' tokens_chunk(toks, size = 5, discard_remainder = FALSE)
tokens_chunk <- function(x, size, overlap = FALSE, discard_remainder = TRUE) {
    UseMethod("tokens_chunk")
}


#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_chunk.tokens <- function(x, size, overlap = FALSE, discard_remainder = TRUE) {

    if (length(size) > 1L)
        stop("Size must be a single integer")
    if (size < 1L)
        stop("Chunk size must be larger than or equal to 1")

    attrs <- attributes(x)
    type <- types(x)
    result <- qatd_cpp_tokens_chunk(x, type, size, overlap)

    # add repeated versions of remaining docvars
    attrs$docvars <- attrs$docvars[attr(result, "docnum"),, drop = FALSE] # repeat rows
    if (any(duplicated(attr(result, "docnum")))) {
        docid <- paste0(names(x)[attr(result, "docnum")], ".", attr(result, "segnum"))
    } else {
        docid <- names(x)
    }
    attrs$docvars["_document"] <- names(x)[attr(result, "docnum")]
    attrs$docvars["_docid"] <- attr(result, "docnum")
    attrs$docvars["_segid"] <- attr(result, "segnum")

    attrs$names <- rownames(attrs$docvars) <- docid
    attr(result, "docnum") <- attr(result, "segnum") <- NULL
    attributes(result) <- attrs

    if (discard_remainder && length(result[[ndoc(result)]]) < size) {
        result <- result[-ndoc(result)]
    }

    result
}

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
