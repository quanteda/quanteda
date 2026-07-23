#' Remove redundant or unused tokens IDs
#'
#' This function reassign tokens IDs after changes in removal or transformation of 
#' tokens. It update token IDs to ensure that they are unique and dense. 
#' Usually, this function is only applied to [tokens_xptr] objects at the end of 
#' text pre-processing.
#' @param x the [tokens_xptr] object.
#' @param force if `TRUE`, update tokens IDs even when it may not be necessary.
#' @keywords tokens internal
#' @export
#' @examples
#' toks <- tokens(c(one = "a b c d A B C D",
#'                  two = "A B C d"), xptr = TRUE)
#' toks <- tokens_tolower(toks)
#' types(toks)
#' types(tokens_recompile(toks))
tokens_recompile <- function(x, force = FALSE) {
    UseMethod("tokens_recompile")
}

#' @export
tokens_recompile.default <- function(x, force = FALSE) {
    check_class(class(x), "tokens_recompile")
}

#' @export
tokens_recompile.tokens_xptr <- function(x, force = FALSE) {
    cpp_recompile(x, force = force)
}

#' @export
tokens_recompile.tokens <- function(x, force = FALSE) {
    as.tokens(tokens_recompile(as.tokens_xptr(x), force))
}