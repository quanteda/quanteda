#' Segment tokens object by patterns
#' 
#' Segment tokens by splitting on a pattern match. This is useful for breaking
#' the tokenized texts into smaller document units, based on a regular pattern
#' or a user-supplied annotation. While it normally makes more sense to do this
#' at the corpus level (see \code{\link{corpus_segment}}), \code{tokens_segment}
#' provides the option to perform this operation on tokens.
#' @param x \link{tokens} object whose token elements will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param extract_pattern remove matched patterns from the texts and save in
#'   \link{docvars}, if \code{TRUE}
#' @param pattern_position either \code{"before"} or \code{"after"}, depending 
#'   on whether the pattern precedes the text (as with a tag) or follows the 
#'   text (as with punctuation delimiters)
#' @param use_docvars if \code{TRUE}, repeat the docvar values for each 
#'   segmented text; if \code{FALSE}, drop the docvars in the segmented corpus. 
#'   Dropping the docvars might be useful in order to conserve space or if these
#'   are not desired for the segmented corpus.
#' @return \code{tokens_segment} returns a \link{tokens} object whose documents 
#'   have been split by patterns
#' @keywords tokens internal
#' @export
#' @examples 
#' txts <- "Fellow citizens, I am again called upon by the voice of my country to
#' execute the functions of its Chief Magistrate. When the occasion proper for
#' it shall arrive, I shall endeavor to express the high sense I entertain of
#' this distinguished honor."
#' toks <- tokens(txts)
#' 
#' # split by any punctuation
#' tokens_segment(toks, "^\\p{Sterm}$", valuetype = "regex", 
#'                extract_pattern = TRUE, 
#'                pattern_position = "after")
#' tokens_segment(toks, c(".", "?", "!"), valuetype = "fixed", 
#'                extract_pattern = TRUE, 
#'                pattern_position = "after")
tokens_segment <- function(x, pattern,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE, 
                           extract_pattern = FALSE,
                           pattern_position = c("before", "after"),
                           use_docvars = TRUE) {
    UseMethod("tokens_segment")
}


#' @rdname tokens_segment
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_segment.tokens <- function(x, pattern,
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE, 
                                  extract_pattern = FALSE,
                                  pattern_position = c("before", "after"),
                                  use_docvars = TRUE) {
    
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    
    attrs <- attributes(x)
    types <- types(x)
    vars <- docvars(x)

    ids <- pattern2list(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    if ("" %in% pattern) ids <- c(ids, list(0)) # append padding index

    if (pattern_position == "before") {
        x <- qatd_cpp_tokens_segment(x, types, ids, extract_pattern, 1)
    } else {
        x <- qatd_cpp_tokens_segment(x, types, ids, extract_pattern, 2)
    }
    docname <- paste(attr(x, 'document'), as.character(attr(x, 'segid')), sep = '.')
    
    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(vars)) {
        vars <- vars[attr(x, 'docid'),,drop = FALSE] # repeat rows
        rownames(vars) <- docname
    } else {
        attrs$docvars <- NULL
        vars <- NULL
    }
    result <- create(x, what = 'tokens', 
                     attrs = attrs, 
                     docvars = vars, 
                     names = docname,
                     document = NULL, 
                     docid = NULL, 
                     segid = NULL)
    
    docvars(result, '_document') <- attr(x, 'document')
    docvars(result, '_docid') <- attr(x, 'docid')
    docvars(result, '_segid') <- attr(x, 'segid')
    if (extract_pattern) docvars(result, "pattern") <- attr(x, 'pattern')
    
    return(result)

}

#' Segment tokens object by chunks of a given size
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
#' @export
#' @examples 
#' txts <- c(doc1 = "Fellow citizens, I am again called upon by the voice of 
#'                   my country to execute the functions of its Chief Magistrate.",
#'           doc2 = "When the occasion proper for it shall arrive, I shall 
#'                   endeavor to express the high sense I entertain of this
#'                   distinguished honor.")
#' toks <- tokens(txts)
#' tokens_chunk(toks, size = 5)
tokens_chunk <- function(x, size, discard_remainder = TRUE) {
    UseMethod("tokens_chunk")
}

#' @export
tokens_chunk.tokens <- function(x, size, discard_remainder = TRUE) {
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
    
    tokens_segment(x, pattern = septoken, extract_pattern = TRUE)
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
