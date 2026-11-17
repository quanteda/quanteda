#' Segment tokenised documents by pattern
#'
#' Split tokenised documents at matching tokens or phrases, such as sentence
#' punctuation or annotation tags. Optional token limits control where
#' segments can end.
#' @param x a [tokens] or [tokens_xptr] object
#' @inheritParams pattern
#' @inheritParams valuetype
#' @inheritParams messages
#' @param extract_pattern if `TRUE`, remove matched delimiters at segment
#'   boundaries and save them in the `pattern` [document variable][docvars].
#' @param pattern_position `"before"` associates a matched delimiter with the
#'   following segment, as with a tag. `"after"` associates it with the
#'   preceding segment, as with sentence punctuation.
#' @param min_ntoken,max_ntoken positive integers giving the minimum and maximum
#'   segment lengths in tokens. Use `NULL` (the default) to omit either limit.
#'   When both are supplied, `min_ntoken` must be less than or equal to
#'   `max_ntoken`.
#' @param use_docvars if `TRUE`, copy the original document variables to each
#'   segment. If `FALSE`, drop the original document variables.
#' @return An object of the same type as `x`, with documents split into
#'   segments.
#' @keywords tokens
#' @details
#'   Use `min_ntoken` to suppress early pattern boundaries and `max_ntoken` to
#'   force splits by length even when no pattern matches. The final segment of
#'   each document may be shorter than `min_ntoken`.
#'
#'   Retained delimiters count towards the length of the segment they belong to.
#'   Extracted delimiters are excluded from the output token count. When
#'   `extract_pattern = TRUE`, the `pattern` document variable is `NA` for a
#'   segment with no extracted delimiter.
#' @seealso [tokens_chunk()] for chunking by token count with optional overlap,
#'   and [corpus_segment()] for pattern-based segmentation of corpus text.
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
#'
#' # Retain an early full stop, extract the next, and split again by length.
#' toks <- tokens("a . b c . d e f g h i j")
#' segs <- tokens_segment(toks, ".", valuetype = "fixed",
#'                        pattern_position = "after", extract_pattern = TRUE,
#'                        min_ntoken = 4, max_ntoken = 6)
#' ntoken(segs)            # 4, 6, 1
#' docvars(segs, "pattern") # ".", NA, NA
tokens_segment <- function(x, pattern,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           extract_pattern = FALSE,
                           pattern_position = c("before", "after"),
                           min_ntoken = NULL, 
                           max_ntoken = NULL,
                           use_docvars = TRUE,
                           verbose = quanteda_options("verbose")) {
    UseMethod("tokens_segment")
}

#' @export
tokens_segment.default <- function(x, pattern,
                                   valuetype = c("glob", "regex", "fixed"),
                                   case_insensitive = TRUE,
                                   extract_pattern = FALSE,
                                   pattern_position = c("before", "after"),
                                   min_ntoken = NULL, 
                                   max_ntoken = NULL, 
                                   use_docvars = TRUE,
                                   verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_segment")
}

#' @rdname tokens_segment
#' @noRd
#' @export
tokens_segment.tokens_xptr <- function(x, pattern,
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE,
                                  extract_pattern = FALSE,
                                  pattern_position = c("before", "after"),
                                  min_ntoken = NULL, 
                                  max_ntoken = NULL, 
                                  use_docvars = TRUE,
                                  verbose = quanteda_options("verbose")) {

    valuetype <- match.arg(valuetype)
    extract_pattern <- check_logical(extract_pattern)
    pattern_position <- match.arg(pattern_position)
    use_docvars <- check_logical(use_docvars)
    verbose <- check_logical(verbose)
    
    if (!is.null(min_ntoken)) {
        min_ntoken <- check_integer(min_ntoken, min = 1)
    } else {
        min_ntoken <- 1L;
    }
    if (!is.null(max_ntoken)) {
        max_ntoken <- check_integer(max_ntoken, min = 1)
    } else {
        max_ntoken <- .Machine$integer.max;
    }
    if (min_ntoken > max_ntoken)
        stop("max_ntoken must be equal to or greater than min_ntoken")
    
    if (!use_docvars)
        docvars(x) <- NULL
    attrs <- attributes(x)
    type <- get_types(x)
    ids <- object2id(pattern, type, valuetype, case_insensitive,
                     concatenator = field_object(attrs, "concatenator"))
    if ("" %in% pattern) ids <- c(ids, list(0)) # append padding index

    if (pattern_position == "before") {
        result <- cpp_tokens_segment(x, ids, extract_pattern, 1, min_ntoken, max_ntoken, get_threads())
    } else {
        result <- cpp_tokens_segment(x, ids, extract_pattern, 2, min_ntoken, max_ntoken, get_threads())
    }
    field_object(attrs, "unit") <- "segments"
    attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]], 
                                          attr(result, "index"),
                                          drop_docid = FALSE)
    attr(result, "index") <- NULL
    if (extract_pattern) {
        m <- attr(result, "matches")
        b <- !nzchar(m)
        m[b] <- rep(NA_character_, sum(b))
        attrs[["docvars"]][["pattern"]] <- m
    }
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_segment()", stats_tokens(result))
    return(result)
}


#' @export
tokens_segment.tokens <- function(x, ...) {
    as.tokens(tokens_segment(as.tokens_xptr(x), ...))
}

