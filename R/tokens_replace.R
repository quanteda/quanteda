#' Replace tokens in a tokens object
#'
#' Substitute token types based on vectorized one-to-one matching. Since this
#' function is created for lemmatization or user-defined stemming. It supports
#' substitution of multi-word features by multi-word features, but substitution
#' is fastest when `pattern` and `replacement` are character vectors
#' and `valuetype = "fixed"` as the function only substitute types of
#' tokens. Please use [tokens_lookup()] with `exclusive = FALSE`
#' to replace [dictionary] values.
#' @param x [tokens] object whose token elements will be replaced
#' @param pattern a character vector or list of character vectors.  See
#'   [pattern] for more details.
#' @param replacement a character vector or (if `pattern` is a list) list
#'   of character vectors of the same length as `pattern`
#' @inheritParams apply_if
#' @inheritParams valuetype
#' @inheritParams messages
#' @export
#' @seealso tokens_lookup
#' @examples
#' toks1 <- tokens(data_corpus_inaugural, remove_punct = TRUE)
#'
#' # lemmatization
#' taxwords <- c("tax", "taxing", "taxed", "taxed", "taxation")
#' lemma <- rep("TAX", length(taxwords))
#' toks2 <- tokens_replace(toks1, taxwords, lemma, valuetype = "fixed")
#' kwic(toks2, "TAX") |>
#'     tail(10)
#'
#' # stemming
#' type <- types(toks1)
#' stem <- char_wordstem(type, "porter")
#' toks3 <- tokens_replace(toks1, type, stem, valuetype = "fixed", case_insensitive = FALSE)
#' identical(toks3, tokens_wordstem(toks1, "porter"))
#'
#' # multi-multi substitution
#' toks4 <- tokens_replace(toks1, phrase(c("Supreme Court")),
#'                         phrase(c("Supreme Court of the United States")))
#' kwic(toks4, phrase(c("Supreme Court of the United States")))
tokens_replace <- function(x, pattern, replacement, valuetype = "glob",
                           case_insensitive = TRUE, apply_if = NULL,
                           verbose = quanteda_options("verbose")) {
    UseMethod("tokens_replace")
}

#' @export
tokens_replace.default <- function(x, pattern, replacement, valuetype = "glob",
                                   case_insensitive = TRUE, apply_if = NULL,
                                   verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_replace")
}

#' @export
tokens_replace.tokens_xptr <- function(x, pattern, replacement, valuetype = "glob",
                                       case_insensitive = TRUE, apply_if = NULL,
                                       verbose = quanteda_options("verbose")) {

    if (length(pattern) != length(replacement))
        stop("The length of pattern and replacement must be the same", call. = FALSE)
    if (!length(pattern)) return(x)

    apply_if <- check_logical(apply_if, min_len = ndoc(x), max_len = ndoc(x),
                               allow_null = TRUE, allow_na = TRUE)
    verbose <- check_logical(verbose)

    type <- get_types(x)
    attrs <- attributes(x)
    type <- union(type, unlist(replacement, use.names = FALSE))
    conc <- field_object(attrs, "concatenator")

    if (is.null(apply_if))
        apply_if <- rep(TRUE, length.out = ndoc(x))
    if (verbose)
        before <- stats_tokens(x)
    
    ids_pat <- object2id(pattern, type, valuetype, case_insensitive, conc, keep_nomatch = FALSE)
    ids_rep <- object2id(replacement, type, "fixed", FALSE, conc, keep_nomatch = TRUE)
    set_types(x) <- type
    
    result <- cpp_tokens_replace(x, ids_pat, ids_rep[attr(ids_pat, "pattern")], !apply_if,
                                 get_threads())
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_replace()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_replace.tokens <- function(x, ...) {
    as.tokens(tokens_replace(as.tokens_xptr(x), ...))
}

#' Replace types by patterns
#'
#' @noRd
#' @keywords internal
replace_type <- function(type, pattern, replacement, case_insensitive) {

    if (!length(type)) return(character())

    # normalize unicode
    pattern <- stri_trans_nfc(pattern)
    replacement <- stri_trans_nfc(replacement)

    if (case_insensitive) {
        type_new <- replacement[match(stri_trans_tolower(type), stri_trans_tolower(pattern))]
    } else {
        type_new <- replacement[match(type, pattern)]
    }
    return(ifelse(is.na(type_new), type, type_new))
}
