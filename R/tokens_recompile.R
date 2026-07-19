#' Remove redundant or unused tokens IDs
#'
#' This function update tokens IDs after changes in removal or transformation of 
#' tokens. It reassign IDs for tokens to ensure that them are unique and dense. 
#' Usually, this function is only used for [tokens_xptr] object.
#' @param x the [tokens_xptr] object.
#' @export
#' @examples
#' toks <- tokens(c(one = "a b c d A B C D",
#'                  two = "A B C d"), xptr = TRUE)
#' toks <- tokens_tolower(toks)
#' types(toks)
#' types(tokens_recompile(toks))
tokens_recompile <- function(x) {
    UseMethod("tokens_recompile")
}

#' @export
tokens_recompile.default <- function(x) {
    check_class(class(x), "tokens_recompile")
}

#' @export
tokens_recompile.tokens_xptr <- function(x) {
    cpp_recompile(x)
    return(x)
}

#' @export
tokens_recompile.tokens <- function(x) {
    as.tokens(tokens_recompile(as.tokens_xptr(x)))
}