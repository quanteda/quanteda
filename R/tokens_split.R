#' Split tokens by separator
#' @param x tokens object
#' @param separator a character by which tokens are seperated
#' @inheritParams valuetype
#' @param remove_separator if \code{TRUE}, remove separator from new tokens
#' @examples
#' toks <- tokens("UK-EU negotiation is not going anywhere as of 2019-12-24", 
#'                separator = "-")
#' tokens_split(toks) 
#' 
#' @export 
tokens_split <- function(x, separator = " ", valuetype = c("fixed", "regex"), remove_separator = FALSE) {
    UseMethod("tokens_split")
}

#' @export
tokens_split.default <- function(x, separator = " ", valuetype = c("fixed", "regex"), remove_separator = FALSE) {
    stop(friendly_class_undefined_message(class(x), "tokens_split"))
}

#' @export
tokens_split.tokens <- function(x, separator = " ", valuetype = c("fixed", "regex"), remove_separator = FALSE) {
    
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
