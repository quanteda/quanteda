#' bootstrap a dfm
#'
#' Create an array of resampled dfms.
#' @param x the dfm
#' @param n number of resamples
#' @param ... additional arguments passed to \link{\code{dfm}}
#' @export
bootstrap_dfm <- function(x, n = 10, ...) {
    UseMethod("dfm_bootstrap")
}

#' @noRd
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ...) {
    
    for (i in 1:n) {
        corp_sentences <- corpus_reshape(x, to = "sentences")
        corp_sentences <- corpus_resample(corp_senteces, replace = TRUE)
        tmp <- corpus_reshape(x, to = "documents")
    }

}

#' @noRd
#' @export
bootstrap_dfm.character <- function(x, n = 10, ...) {
    dfm_bootstrap(corpus(x), ...)
}



