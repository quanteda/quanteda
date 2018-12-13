#' Segment tokens object by chunks of a given size
#' 
#' Segment tokens by splitting into equally sized chunks.
#' @param x \link{tokens} object whose token elements will be segmented into
#'   chunks
#' @param size integer; the size of the chunks in tokens
#' @param overlap logical; if \code{TRUE}, the last token of a chunk will be the
#'   first token of the next chunk
#' @param keep_remainder logical; if \code{TRUE}, keep any ending chunk whose
#'   length is less than \code{size}, otherwise this will be discarded
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
#' tokens_chunk(toks, size = 5, keep_remainder = TRUE)
tokens_chunk <- function(x, size, overlap = FALSE, keep_remainder = FALSE) {
    UseMethod("tokens_chunk")
}


#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_chunk.tokens <- function(x, size, overlap = FALSE, keep_remainder = FALSE) {

    if (length(size) > 1L)
        stop("Size must be a single integer")
    if (size < 1L)
        stop("Chunk size must be larger than or equal to 1")

    attrs <- attributes(x)
    type <- types(x)
    result <- qatd_cpp_tokens_chunk(x, type, size, overlap, !keep_remainder)

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

    result
}
