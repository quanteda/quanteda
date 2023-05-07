#' Copy an entire object
#'
#' Creates a deep copy of an object, as an external pointer, for instance a
#' [tokens_xptr] class object.
#'
#' @param x the input object to be copied
#' @returns an entire copy of `x`, by value
#' @export
#' @examples
#' xtoks <- tokens(data_corpus_inaugural[1:3], xptr = TRUE)
#' print(xtoks, 0, 0)
#' 
#' # shallow copy
#' xtoks_shallow <- xtoks
#' print(xtoks_shallow, 0, 0)
#' 
#' # deep copy
#' xtoks_deep <- copy(xtoks)
#' print(xtoks_deep, 0, 0)
copy <-  function(x) {
    UseMethod("copy")
}

#' @export
copy.default <- function(x) {
    return(x)
}

#' @export
copy.tokens_xptr <- function(x) {
    as.tokens_xptr(x)
}
