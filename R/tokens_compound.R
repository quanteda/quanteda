#' Convert token sequences into compound tokens
#'
#' Replace multi-token sequences with a multi-word, or "compound" token.  The
#' resulting compound tokens will represent a phrase or multi-word expression,
#' concatenated with `concatenator` (by default, the "`_`" character) to form a
#' single "token".  This ensures that the sequences will be processed
#' subsequently as single tokens, for instance in constructing a [dfm].
#' @param x an input [tokens] object
#' @inheritParams pattern
#' @inheritParams valuetype
#' @inheritParams messages
#' @param join logical; if `TRUE`, join overlapping compounds into a single
#'   compound; otherwise, form these separately.  See examples.
#' @param keep_unigrams if `TRUE`, keep the original tokens.
#' @param window integer; a vector of length 1 or 2 that specifies size of the
#'   window of tokens adjacent to `pattern` that will be compounded with matches
#'   to `pattern`.  The window can be asymmetric if two elements are specified,
#'   with the first giving the window size before `pattern` and the second the
#'   window size after.  If paddings (empty `""` tokens) are found, window will
#'   be shrunk to exclude them.
#' @param concatenator character; the concatenation character that will connect
#'   the tokens making up a multi-token sequence.
#' @inheritParams apply_if
#' @return A [tokens] object in which the token sequences matching `pattern`
#'   have been replaced by new compounded "tokens" joined by the concatenator.
#' @note Patterns to be compounded (naturally) consist of multi-word sequences,
#'   and how these are expected in `pattern` is very specific.  If the elements
#'   to be compounded are supplied as space-delimited elements of a character
#'   vector, wrap the vector in [phrase()].  If the elements to be compounded
#'   are separate elements of a character vector, supply it as a list where each
#'   list element is the sequence of character elements.
#'
#'   See the examples below.
#' @export
#' @examples
#' txt <- "The United Kingdom is leaving the European Union."
#' toks <- tokens(txt, remove_punct = TRUE)
#'
#' # character vector - not compounded
#' tokens_compound(toks, c("United", "Kingdom", "European", "Union"))
#'
#' # elements separated by spaces - not compounded
#' tokens_compound(toks, c("United Kingdom", "European Union"))
#'
#' # list of characters - is compounded
#' tokens_compound(toks, list(c("United", "Kingdom"), c("European", "Union")))
#'
#' # elements separated by spaces, wrapped in phrase() - is compounded
#' tokens_compound(toks, phrase(c("United Kingdom", "European Union")))
#'
#' # supplied as values in a dictionary (same as list) - is compounded
#' # (keys do not matter)
#' tokens_compound(toks, dictionary(list(key1 = "United Kingdom",
#'                                       key2 = "European Union")))
#' # pattern as dictionaries with glob matches
#' tokens_compound(toks, dictionary(list(key1 = c("U* K*"))), valuetype = "glob")
#'
#' # note the differences caused by join = FALSE
#' compounds <- list(c("the", "European"), c("European", "Union"))
#' tokens_compound(toks, pattern = compounds, join = TRUE)
#' tokens_compound(toks, pattern = compounds, join = FALSE)
#'
#' # use window to form ngrams
#' tokens_remove(toks, pattern = stopwords("en")) |>
#'     tokens_compound(pattern = "leav*", join = FALSE, window = c(0, 3))
#'
tokens_compound <- function(x, pattern,
                            valuetype = c("glob", "regex", "fixed"),
                            concatenator = concat(x),
                            window = 0L,
                            case_insensitive = TRUE, 
                            join = TRUE,
                            keep_unigrams = FALSE,
                            apply_if = NULL,
                            verbose = quanteda_options("verbose")) {
    UseMethod("tokens_compound")
}

#' @export
tokens_compound.default <- function(x, pattern,
                                    valuetype = c("glob", "regex", "fixed"),
                                    concatenator = concat(x),
                                    window = 0L,
                                    case_insensitive = TRUE, 
                                    join = TRUE,
                                    keep_unigrams = FALSE,
                                    apply_if = NULL,
                                    verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_compound")
}

#' @export
tokens_compound.tokens_xptr <- function(x, pattern,
                                        valuetype = c("glob", "regex", "fixed"),
                                        concatenator = concat(x),
                                        window = 0L,
                                        case_insensitive = TRUE, 
                                        join = TRUE,
                                        keep_unigrams = FALSE,
                                        apply_if = NULL,
                                        verbose = quanteda_options("verbose")) {

    valuetype <- match.arg(valuetype)
    concatenator <- check_character(concatenator)
    window <- check_integer(window, min_len = 1, max_len = 2, min = 0)
    join <- check_logical(join)
    apply_if <- check_logical(apply_if, min_len = ndoc(x), max_len = ndoc(x),
                               allow_null = TRUE, allow_na = TRUE)
    verbose <- check_logical(verbose)
    keep_unigrams <- check_logical(keep_unigrams)

    attrs <- attributes(x)
    type <- get_types(x)

    ids <- object2id(pattern, type, valuetype, case_insensitive, remove_unigram = all(window == 0))
    if (length(window) == 1) window <- rep(window, 2)
    if (is.null(apply_if))
        apply_if <- rep(TRUE, length.out = ndoc(x))
    if (verbose)
        before <- stats_tokens(x)
    result <- cpp_tokens_compound(x, ids, concatenator, join, keep_unigrams,
                                  window[1], window[2], !apply_if, get_threads())
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_compound()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_compound.tokens <- function(x, ...) {
    as.tokens(tokens_compound(as.tokens_xptr(x), ...))
}
