#' [Experimental] Chunk documents in equal sizes
#' 
#' Segment tokens by splitting documents in equal sizes.
#' @param x \link{tokens} object whose token elements will be segmented
#' @param size size of each chunk
#' @param overlap if \code{TRUE}, chunks overlaps with next chunks
#' @return returns a \link{tokens} object whose documents are in the same length
#' @keywords tokens
#' @export
#' @examples 
#' txt <- "Fellow citizens, I am again called upon by the voice of my country to
#' execute the functions of its Chief Magistrate. When the occasion proper for
#' it shall arrive, I shall endeavor to express the high sense I entertain of
#' this distinguished honor."
#' toks <- tokens(txt)
#' tokens_chunk(toks, 5)
#' tokens_chunk(toks, 5, TRUE)
tokens_chunk <- function(x, size, overlap = FALSE) {
    UseMethod("tokens_chunk")
}


#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_chunk.tokens <- function(x, size, overlap = FALSE) {
    
    if (length(size) > 1L) 
        stop("Size must be a single integer")
    if (size < 1L) 
        stop("Chunk size must be larger than or equal to 1")

    attrs <- attributes(x)
    type <- types(x)    
    result <- qatd_cpp_tokens_chunk(x, type, size, overlap)
    
    # add repeated versions of remaining docvars
    attrs$docvars <- attrs$docvars[attr(result, "docnum"),,drop = FALSE] # repeat rows
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
