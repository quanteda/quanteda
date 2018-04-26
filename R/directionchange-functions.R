
#' @export
tokens_tortl.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "tokens_tortl"))
}

#' [Experimental] Change direction of words in tokens
#' 
#' This function adds unicode direction mark to tokens types for punctuations
#' and symbols to correct how right-to-left languages (e.g. Arabic, Hebrew,
#' Urdu) are printed in HTML-based consoles (e.g. R Studio). This is an
#' experimental function subjects to future changes.
#' @rdname tokens_tortl
#' @noRd
#' @export
tokens_tortl.tokens <- function(x) {
    attr(x, "types") <- char_tortl(types(x))
    return(x)
}

#' @export
char_tortl.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "char_tortl"))
}

#' @rdname tokens_tortl
#' @noRd
#' @export
char_tortl <- function(x) {
    stri_replace_all_regex(x, "([\\p{P}\\p{S}])", "$1\\u200F")
}
