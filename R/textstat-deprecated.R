#' deprecated textstat names
#' 
#' Deprecated `textstat_` function names.
#' @name deprecated-textstat
#' @keywords internal deprecated
NULL

#' @rdname deprecated-textstat
#' @details \code{lexdiv} is the deprecated; use \code{\link{textstat_lexdiv}} instead.
lexdiv <- function(x, ...) {
    .Deprecated("textstat_lexdiv")
    textstat_lexdiv(x, ...)
}

#' @rdname deprecated-textstat
#' @details \code{readability} is the deprecated; use \code{\link{textstat_readability}} instead.
lexdiv <- function(x, ...) {
    .Deprecated("textstat_readability")
    textstat_readability(x, ...)
}


