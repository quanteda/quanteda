#' Split tokens by a separator pattern
#'
#' Replaces tokens by multiple replacements consisting of elements split by a
#' separator pattern, with the option of retaining the separator.  This function
#' effectively reverses the operation of [tokens_compound()].
#' @param x a [tokens] object
#' @param separator a single-character pattern match by which tokens are separated
#' @inheritParams valuetype
#' @inheritParams messages
#' @param remove_separator if `TRUE`, remove separator from new tokens
#' @inheritParams apply_if
#' @examples
#' # undo tokens_compound()
#' toks1 <- tokens("pork barrel is an idiomatic multi-word expression")
#' tokens_compound(toks1, phrase("pork barrel"))
#' tokens_compound(toks1, phrase("pork barrel")) |>
#'     tokens_split(separator = "_")
#'
#' # similar to tokens(x, remove_hyphen = TRUE) but post-tokenization
#' toks2 <- tokens("UK-EU negotiation is not going anywhere as of 2018-12-24.")
#' tokens_split(toks2, separator = "-", remove_separator = FALSE)
#' @keywords tokens
#' @export
tokens_split <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                         remove_separator = TRUE, apply_if = NULL,
                         verbose = quanteda_options("verbose")) {
    UseMethod("tokens_split")
}

#' @export
tokens_split.default <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                                 remove_separator = TRUE, apply_if = NULL,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_split")
}

#' @export
tokens_split.tokens_xptr <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                                     remove_separator = TRUE, apply_if = NULL,
                                     verbose = quanteda_options("verbose")) {

    separator <- check_character(separator)
    valuetype <- match.arg(valuetype)
    remove_separator <- check_logical(remove_separator)
    verbose <- check_logical(verbose)

    type <- get_types(x)
    if (valuetype == "regex") {
        type <- type[stri_detect_regex(type, separator)]
    } else {
        type <- type[stri_detect_fixed(type, separator)]
    }

    pattern <- as.list(type)
    if (length(pattern) == 0) return(x)

    if (valuetype == "regex") {
        if (remove_separator) {
            type <- stri_replace_all_regex(type, separator, "\uE000")
        } else {
            type <- stri_replace_all_regex(type, separator, "\uE000$0\uE000")
        }
    } else {
        if (remove_separator) {
            type <- stri_replace_all_fixed(type, separator, "\uE000")
        } else {
            type <- stri_replace_all_fixed(type, separator, stri_c("\uE000", separator, "\uE000"))
        }
    }

    replacement <- stri_split_fixed(type, "\uE000", omit_empty = TRUE)
    if (verbose)
        before <- stats_tokens(x)
    result <- tokens_replace(x, pattern, replacement, "fixed", case_insensitive = FALSE,
                             apply_if = apply_if, verbose = FALSE)
    if (verbose)
        message_tokens("tokens_split()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_split.tokens <- function(x, ...) {
    as.tokens(tokens_split(as.tokens_xptr(x), ...))
}
