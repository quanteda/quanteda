#' Select or remove tokens from a tokens object
#'
#' These function select or discard tokens from a [tokens] object.  For
#' convenience, the functions `tokens_remove` and `tokens_keep` are defined as
#' shortcuts for `tokens_select(x, pattern, selection = "remove")` and
#' `tokens_select(x, pattern, selection = "keep")`, respectively.  The most
#' common usage for `tokens_remove` will be to eliminate stop words from a text
#' or text-based object, while the most common use of `tokens_select` will be to
#' select tokens with only positive pattern matches from a list of regular
#' expressions, including a dictionary. `startpos` and `endpos` determine the
#' positions of tokens searched for `pattern` and areas affected are expanded by
#' `window`.
#' @param x [tokens] object whose token elements will be removed or kept
#' @inheritParams pattern
#' @param selection whether to `"keep"` or `"remove"` the tokens matching
#'   `pattern`
#' @inheritParams valuetype
#' @inheritParams messages
#' @param padding if `TRUE`, leave an empty string where the removed tokens
#'   previously existed.  This is useful if a positional match is needed between
#'   the pre- and post-selected tokens, for instance if a window of adjacency
#'   needs to be computed.
#' @param window integer of length 1 or 2; the size of the window of tokens
#'   adjacent to `pattern` that will be selected. The window is symmetric unless
#'   a vector of two elements is supplied, in which case the first element will
#'   be the token length of the window before `pattern`, and the second will be
#'   the token length of the window after `pattern`. The default is `0`, meaning
#'   that only the pattern matched token(s) are selected, with no adjacent
#'   terms.
#'
#'   Terms from overlapping windows are never double-counted, but simply
#'   returned in the pattern match. This is because `tokens_select` never
#'   redefines the document units; for this, see [kwic()].
#' @param startpos,endpos integer; position of tokens in documents where pattern
#'   matching starts and ends, where 1 is the first token in a document.  For
#'   negative indexes, counting starts at the ending token of the document, so
#'   that -1 denotes the last token in the document, -2 the second to last, etc.
#'   When the length of the vector is equal to `ndoc`, tokens in corresponding
#'   positions will be selected; when it is less than `ndoc`, values are
#'   repeated to make them equal in length.
#' @param min_nchar,max_nchar optional numerics specifying the minimum and
#'   maximum length in characters for tokens to be removed or kept; defaults are
#'   `NULL` for no limits.  These are applied after (and hence, in addition to)
#'   any selection based on pattern matches.
#' @inheritParams apply_if
#' @inheritParams messages
#' @return a [tokens] object with tokens selected or removed based on their
#'   match to `pattern`
#' @export
#' @examples
#' ## tokens_select with simple examples
#' toks <- as.tokens(list(letters, LETTERS))
#' tokens_select(toks, c("b", "e", "f"), selection = "keep", padding = FALSE)
#' tokens_select(toks, c("b", "e", "f"), selection = "keep", padding = TRUE)
#' tokens_select(toks, c("b", "e", "f"), selection = "remove", padding = FALSE)
#' tokens_select(toks, c("b", "e", "f"), selection = "remove", padding = TRUE)
#'
#' # how case_insensitive works
#' tokens_select(toks, c("b", "e", "f"), selection = "remove", case_insensitive = TRUE)
#' tokens_select(toks, c("b", "e", "f"), selection = "remove", case_insensitive = FALSE)
#'
#' # use window
#' tokens_select(toks, c("b", "f"), selection = "keep", window = 1)
#' tokens_select(toks, c("b", "f"), selection = "remove", window = 1)
#' tokens_remove(toks, c("b", "f"), window = c(0, 1))
#' tokens_select(toks, pattern = c("e", "g"), window = c(1, 2))
#'
tokens_select <- function(x, pattern, selection = c("keep", "remove"),
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, window = 0,
                          min_nchar = NULL, max_nchar = NULL,
                          startpos = 1L, endpos = -1L,
                          apply_if = NULL,
                          verbose = quanteda_options("verbose")) {
    UseMethod("tokens_select")
}

#' @export
tokens_select.default <- function(x, pattern = NULL,
                                  selection = c("keep", "remove"),
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE, padding = FALSE, window = 0,
                                  min_nchar = NULL, max_nchar = NULL,
                                  startpos = 1L, endpos = -1L,
                                  apply_if = NULL,
                                  verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_select")
}

#' @rdname tokens_select
#' @noRd
#' @export
#' @examples
#' toks <- tokens(c(doc1 = "This is a SAMPLE text", doc2 = "this sample text is better"))
#' feats <- c("this", "sample", "is")
#'
#' # keeping tokens
#' tokens_select(toks, feats, selection = "keep")
#' tokens_select(toks, feats, selection = "keep", padding = TRUE)
#' tokens_select(toks, feats, selection = "keep", case_insensitive = FALSE)
#' tokens_select(toks, feats, selection = "keep", padding = TRUE, case_insensitive = FALSE)
#'
#' # removing tokens
#' tokens_select(toks, feats, selection = "remove")
#' tokens_select(toks, feats, selection = "remove", padding = TRUE)
#' tokens_select(toks, feats, selection = "remove", case_insensitive = FALSE)
#' tokens_select(toks, feats, selection = "remove", padding = TRUE, case_insensitive = FALSE)
#'
#' # with longer texts
#' toks2 <- tokens(data_corpus_inaugural)
#' tokens_select(toks2, stopwords("english"), "remove")
#' tokens_select(toks2, stopwords("english"), "keep")
#' tokens_select(toks2, stopwords("english"), "remove", padding = TRUE)
#' tokens_select(toks2, stopwords("english"), "keep", padding = TRUE)
#'
#' # with multiple words
#' tokens_select(toks2, list(c("President", "*")), "keep")
#' tokens_select(toks2, "President *", "keep") # simplified form
#' tokens_select(toks2, list(c("*", "crisis")), "keep")
#' tokens_select(toks2, "* crisis", "keep") # simplified form
#'
#' # with minimum length
#' tokens_select(toks2, min_nchar = 2, "keep") # simplified form
#'
#' # with starting and ending positions
#' tokens_select(toks, "*", startpos = 3)  # exclude first two tokens
#' tokens_select(toks, "*", endpos = 3)    # include only first 3 tokens
#' tokens_select(toks, "*", startpos = -3) # include only last 2 tokens
#'
#' # combining positional selection with pattern matching
#' tokens_select(toks, "t*", endpos = 3)
#'
tokens_select.tokens_xptr <- function(x, pattern = NULL,
                                      selection = c("keep", "remove"),
                                      valuetype = c("glob", "regex", "fixed"),
                                      case_insensitive = TRUE, padding = FALSE, window = 0,
                                      min_nchar = NULL, max_nchar = NULL,
                                      startpos = 1L, endpos = -1L,
                                      apply_if = NULL,
                                      verbose = quanteda_options("verbose")) {

    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    padding <- check_logical(padding)
    window <- check_integer(window, min_len = 1, max_len = 2, min = 0)
    startpos <- check_integer(startpos, max_len = pmax(1, ndoc(x)))
    endpos <- check_integer(endpos, max_len = pmax(1, ndoc(x)))
    apply_if <- check_logical(apply_if, min_len = ndoc(x), max_len = ndoc(x),
                               allow_null = TRUE, allow_na = TRUE)
    verbose <- check_logical(verbose)

    attrs <- attributes(x)
    type <- get_types(x)

    # selection by pattern
    if (is.null(pattern)) {
        if (selection == "keep") {
            ids <- as.list(seq_along(type))
        } else {
            ids <- list()
        }
    } else {
        ids <- object2id(pattern, type, valuetype, case_insensitive,
                         field_object(attrs, "concatenator"))
    }

    # selection by nchar
    if (!is.null(min_nchar) || !is.null(max_nchar)) {
        len <- stri_length(type)
        is_short <- is_long <- rep(FALSE, length(len))
        if (!is.null(min_nchar)) {
            min_nchar <- check_integer(min_nchar, min = 0)
            is_short <- len < min_nchar
        }
        if (!is.null(max_nchar)) {
            max_nchar <- check_integer(max_nchar, min = 0)
            is_long <- max_nchar < len
        }
        id_out <- which(is_short | is_long)
        if (length(id_out)) {
            if (selection == "keep") {
                if (all(lengths(ids) == 1)) {
                    ids <- setdiff(ids, as.list(id_out))
                } else {
                    has_out <- unlist_integer(lapply(ids, function(x, y) length(intersect(x, y)) > 0, id_out))
                    ids <- ids[!has_out]
                }
            } else {
                ids <- union(ids, as.list(id_out))
            }
        }
    }

    if (length(window) == 1) window <- rep(window, 2)
    startpos <- rep(startpos, length.out = ndoc(x))
    endpos <- rep(endpos, length.out = ndoc(x))

    if (is.null(apply_if))
        apply_if <- rep(TRUE, length.out = ndoc(x))
    
    if (verbose)
        before <- stats_tokens(x)
    if (selection == "keep") {
        result <- cpp_tokens_select(x, ids, 1, padding, window[1], window[2], startpos, endpos, !apply_if,
                                    get_threads())
    } else {
        result <- cpp_tokens_select(x, ids, 2, padding, window[1], window[2], startpos, endpos, !apply_if,
                                    get_threads())
    }
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens(ifelse(selection == "keep", "tokens_keep()", "tokens_remove()"), 
                              before, stats_tokens(result))
    return(result)
}


#' @export
tokens_select.tokens <- function(x, ...) {
    as.tokens(tokens_select(as.tokens_xptr(x), ...))
}

#' @rdname tokens_select
#' @param ... additional arguments passed by `tokens_remove` and
#'   `tokens_keep` to `tokens_select`. Cannot include
#'   `selection`.
#' @export
#' @examples
#' # tokens_remove example: remove stopwords
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my
#'                    country to execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall
#'                    endeavor to express the high sense I entertain of this
#'                    distinguished honor.")
#' tokens_remove(tokens(txt, remove_punct = TRUE), stopwords("english"))
#'
tokens_remove <- function(x, ...) {
    (function(..., selection) {
        if (!missing(selection))
            stop("tokens_remove cannot include selection argument")
        tokens_select(x, ..., selection = "remove")
    })(...)
}

#' @rdname tokens_select
#' @export
#' @examples
#' # token_keep example: keep two-letter words
#' tokens_keep(tokens(txt, remove_punct = TRUE), "??")
#'
tokens_keep <- function(x, ...) {
    (function(..., selection) {
        if (!missing(selection))
            stop("tokens_keep cannot include selection argument")
        tokens_select(x, ..., selection = "keep")
    })(...)
}
