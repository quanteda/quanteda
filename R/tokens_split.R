#' Split tokens by a separator pattern
#' 
#' Replaces tokens by multiple replacements consisting of elements split by a
#' separator pattern, with the option of retaining the separator.  This function
#' effectively reverses the operation of \code{\link{tokens_compound}}.
#' @param x a \link{tokens} object
#' @param separator a character by which tokens are separated
#' @inheritParams valuetype
#' @param remove_separator if \code{TRUE}, remove separator from new tokens
#' @examples
#' toks <- tokens("UK-EU negotiation is not going anywhere as of 2018-12-24.")
#' tokens_split(toks, separator = "-") 
#' 
#' ctoks <- tokens("pork barrel is amn idiomatic multi-word expression")
#' tokens_compound(ctoks, phrase("pork barrel"))
#' tokens_compound(ctoks, phrase("pork barrel")) %>%
#'     tokens_split(separator = "_", remove_separator = TRUE)
#' @export 
tokens_split <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                         remove_separator = FALSE) {
    UseMethod("tokens_split")
}

#' @export
tokens_split.default <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                                 remove_separator = FALSE) {
    stop(friendly_class_undefined_message(class(x), "tokens_split"))
}

#' @export
tokens_split.tokens <- function(x, separator = " ", valuetype = c("fixed", "regex"),
                                remove_separator = FALSE) {

    valuetype <- match.arg(valuetype)
    if (length(separator) != 1)
        stop("separator must be a character")

    type <- types(x)
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
    tokens_replace(x, pattern, replacement, "fixed", case_insensitive = FALSE)
}
