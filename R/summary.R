#' Summarize a corpus
#'
#' Displays information about a corpus, including attributes and metadata such
#' as date of number of texts, creation and source.
#'
#' @param object an object to be summarized.
#' @param ... not used.
#' @return a invisible list of summary statistics.
#' @examples
#' corp <- data_corpus_inaugural
#' summary(corp)
#' toks <- tokens(data_corpus_inaugural)
#' summary(toks)
#' dfmt <- dfm(toks)
#' summary(dfmt)
#' fcmt <- fcm(dfmt)
#' summary(fcmt)
#' @export
#' @method summary corpus
summary.corpus <- function(object, ...) {
    invisible(summary_corpus(as.corpus(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary tokens
summary.tokens <- function(object, ...) {
    invisible(summary_tokens(as.tokens(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary dfm
summary.dfm <- function(object, ...) {
    invisible(summary_dfm(as.dfm(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary fcm
summary.fcm <- function(object, ...) {
    invisible(summary_fcm(as.fcm(object)))
}

#' @rdname summary.corpus
#' @export
setMethod("summary", signature(object = "dfm"), summary.dfm)

#' @rdname summary.corpus
#' @export
setMethod("summary", signature(object = "fcm"), summary.fcm)
