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
#' tokens_segment(toks, "^\\\\p{Sterm}$", valuetype = "regex", 
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

    ids <- pattern2list(pattern, types, valuetype, case_insensitive, attr(x, "concatenator"))
    if ("" %in% pattern) ids <- c(ids, list(0)) # append padding index

    if (pattern_position == "before") {
        x <- qatd_cpp_tokens_segment(x, types, ids, extract_pattern, 1)
    } else {
        x <- qatd_cpp_tokens_segment(x, types, ids, extract_pattern, 2)
    }
    docname <- paste(attr(x, "document"), as.character(attr(x, "segid")), sep = ".")

    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(vars)) {
        vars <- vars[attr(x, "docid"), , drop = FALSE] # repeat rows
        rownames(vars) <- docname
    } else {
        vars <- data.frame(row.names = docname)
    }
    result <- create(x, what = "tokens",
                     docvars = vars,
                     names = docname,
                     document = NULL,
                     docid = NULL,
                     segid = NULL)
    
    docvars(result, "_document") <- attr(x, "document")
    docvars(result, "_docid") <- attr(x, "docid")
    docvars(result, "_segid") <- attr(x, "segid")
    if (extract_pattern) docvars(result, "pattern") <- attr(x, "pattern")

    result
}
