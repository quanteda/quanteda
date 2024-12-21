#' Segment tokens object by chunks of a given size
#'
#' Segment tokens into new documents of equally sized token lengths, with the
#' possibility of overlapping the chunks.
#' @param x [tokens] object whose token elements will be segmented into
#'   chunks
#' @param size integer; the token length of the chunks
#' @param overlap integer; the number of tokens in a chunk to be taken from the
#'   last `overlap` tokens from the preceding chunk
#' @param use_docvars if `TRUE`, repeat the docvar values for each chunk;
#'   if `FALSE`, drop the docvars in the chunked tokens
#' @inheritParams messages
#' @return A [tokens] object whose documents have been split into chunks of
#'   length `size`.
#' @seealso [tokens_segment()]
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
#' tokens_chunk(toks, size = 5, overlap = 4)
tokens_chunk <- function(x, size, overlap = 0, use_docvars = TRUE,
                         verbose = quanteda_options("verbose")) {
    UseMethod("tokens_chunk")
}

#' @export
tokens_chunk.default <- function(x, size, overlap = 0, use_docvars = TRUE,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_chunk")
}

#' @noRd
#' @export
tokens_chunk.tokens_xptr <- function(x, size, overlap = 0, use_docvars = TRUE,
                                     verbose = quanteda_options("verbose")) {

    size <- check_integer(size, min = 1)
    overlap <- check_integer(overlap, min = 0)
    use_docvars <- check_logical(use_docvars)
    verbose <- check_logical(verbose)
        
    if (overlap >= size)
        stop("The value of overlap must be smaller than size")
    if (!use_docvars)
        docvars(x) <- NULL

    attrs <- attributes(x)
    if (verbose)
        before <- stats_tokens(x)
    result <- cpp_tokens_chunk(x, size, overlap, get_threads())
    if (any(duplicated(attr(result, "index")))) {
        field_object(attrs, "unit") <- "segments"
    } else {
        field_object(attrs, "unit") <- "documents"
    }
    attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]],
                                          attr(result, "index"), 
                                          drop_docid = FALSE)
    attr(result, "index") <- NULL
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_chunk()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_chunk.tokens <- function(x, ...) {
    as.tokens(tokens_chunk(as.tokens_xptr(x), ...))
}
