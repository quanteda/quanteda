#' Segment tokens object by chunks of a given size
#'
#' Segment tokens into new documents of equally sized token lengths, with the
#' possibility of overlapping the chunks.
#' @param x \link{tokens} object whose token elements will be segmented into
#'   chunks
#' @param size integer; the token length of the chunks
#' @param overlap integer; the number of tokens in a chunk to be taken from the
#'   last \code{overlap} tokens from the preceding chunk
#' @param use_docvars if \code{TRUE}, repeat the docvar values for each chunk;
#'   if \code{FALSE}, drop the docvars in the chunked tokens
#' @return A \link{tokens} object whose documents have been split into chunks of
#'   length \code{size}.
#' @seealso \code{\link{tokens_segment}}
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
tokens_chunk <- function(x, size, overlap = 0, use_docvars = TRUE) {
    UseMethod("tokens_chunk")
}


#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_chunk.tokens <- function(x, size, overlap = 0, use_docvars = TRUE) {

    if (length(size) > 1L)
        stop("Size must be a single integer")
    if (size < 1L)
        stop("Chunk size must be larger than or equal to 1")
    if (overlap < 0L)
        stop("Overlap must be a positive value")
    if (overlap >= size)
        stop("Overlap must be smaller than size")
    if (!use_docvars)
        docvars(x) <- NULL

    attrs <- attributes(x)
    type <- types(x)
    result <- qatd_cpp_tokens_chunk(x, type, size, overlap)

    # add repeated versions of remaining docvars
    attrs$docvars <- attrs$docvars[attr(result, "docnum"), , drop = FALSE] # repeat rows
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

    return(result)
}
