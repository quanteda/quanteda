#' Segment tokens object by patterns
#'
#' Segment tokens by splitting on a pattern match. This is useful for breaking
#' the tokenized texts into smaller document units, based on a regular pattern
#' or a user-supplied annotation. While it normally makes more sense to do this
#' at the corpus level (see [corpus_segment()]), `tokens_segment`
#' provides the option to perform this operation on tokens.
#' @param x [tokens] object whose token elements will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param extract_pattern remove matched patterns from the texts and save in
#'   [docvars], if `TRUE`
#' @param pattern_position either `"before"` or `"after"`, depending
#'   on whether the pattern precedes the text (as with a tag) or follows the
#'   text (as with punctuation delimiters)
#' @param use_docvars if `TRUE`, repeat the docvar values for each
#'   segmented text; if `FALSE`, drop the docvars in the segmented corpus.
#'   Dropping the docvars might be useful in order to conserve space or if these
#'   are not desired for the segmented corpus.
#' @return `tokens_segment` returns a [tokens] object whose documents
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

    x <- as.tokens(x)
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    if (!use_docvars)
        docvars(x) <- NULL
    attrs <- attributes(x)
    type <- types(x)

    ids <- pattern2list(pattern, type, valuetype, case_insensitive,
                        field_object(attrs, "concatenator"))
    if ("" %in% pattern) ids <- c(ids, list(0)) # append padding index

    if (pattern_position == "before") {
        result <- qatd_cpp_tokens_segment(x, type, ids, extract_pattern, 1)
    } else {
        result <- qatd_cpp_tokens_segment(x, type, ids, extract_pattern, 2)
    }
    attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]], attr(result, "docnum"))
    field_object(attrs, "unit") <- "segments"
    if (extract_pattern)
        attrs[["docvars"]][["pattern"]] <- attr(result, "pattern")
    rebuild_tokens(result, attrs)
}
