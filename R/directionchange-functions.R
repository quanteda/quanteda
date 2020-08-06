#' \[Experimental\] Change direction of words in tokens
#' 
#' This function adds a Unicode direction mark to tokens types for punctuations
#' and symbols to correct how right-to-left languages (e.g. Arabic, Hebrew,
#' Persian, and Urdu) are printed in HTML-based consoles (e.g. R Studio). This
#' is an experimental function subject to future change.
#' @param x the input object whose punctuation marks will be modified by the
#'   direction mark
#' @keywords experimental tokens character
#' @export
tokens_tortl <- function(x) {
    UseMethod("tokens_tortl")
}

#' @export
tokens_tortl.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "tokens_tortl"))
}

#' @export
tokens_tortl.tokens <- function(x) {
    attr(x, "types") <- char_tortl(types(x))
    return(x)
}

#' @rdname tokens_tortl
#' @export
char_tortl <- function(x) {
    UseMethod("char_tortl")
}

#' @export
char_tortl.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "char_tortl"))
}

#' @export
char_tortl.character <- function(x) {
    stri_replace_all_regex(x, "([\\p{P}\\p{S}])", "$1\\u200F")
}
