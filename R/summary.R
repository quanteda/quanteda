#' @export
#' @method summary corpus
summary.corpus <- function(x, ...) {
    summary_corpus(x)
}

#' @export
#' @method summary tokens
summary.tokens <- function(x, ...) {
    summary_tokens(x)
}

#' @export
#' @method summary dfm
summary.dfm <- function(x, ...) {
    summary_dfm(x)
}

#' @export
#' @method summary fcm
summary.fcm <- function(x, ...) {
    summary_fcm(x)
}
