#' Split tokenised documents into chunks
#'
#' Split each document into chunks of up to `size` tokens. The start of each
#' successive chunk advances by `size - overlap` tokens. Chunks near the end
#' of a document can be shorter than `size`.
#' @param x a [tokens] or [tokens_xptr] object
#' @param size positive integer giving the number of tokens in a full chunk
#' @param overlap non-negative integer smaller than `size` giving the number
#'   of tokens shared by successive full chunks
#' @param use_docvars if `TRUE`, copy the original document variables to each
#'   chunk. If `FALSE`, drop the original document variables.
#' @inheritParams messages
#' @return An object of the same type as `x`, with documents split into chunks.
#' @seealso [tokens_segment()] for pattern-based segmentation and
#'   [corpus_chunk()] for chunking corpus text.
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
    result <- cpp_tokens_chunk(x, size, overlap, get_threads())
    if (anyDuplicated(attr(result, "index"))) {
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
        message_tokens("tokens_chunk()", stats_tokens(result))
    return(result)
}

#' @export
tokens_chunk.tokens <- function(x, ...) {
    as.tokens(tokens_chunk(as.tokens_xptr(x), ...))
}
