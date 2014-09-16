#' @export
wordcloud <- function(x, ...) {
    UseMethod("wordcloud")
}

#' @export
wordcloud.default <- function(x, ...) {
    wordcloud::wordcloud(x, ...)
}

#' Plot a word cloud for a \link{dfm}
#'
#' plots a document as a wordcloud of its features
#' 
# @aliases wordcloud
#' @param dfm document-feature matrix created in quanteda
#' @param document index of the document whose words will be plotted
#' @param ... additional arguments to pass to \link[wordcloud]{wordcloud} 
#' @return None
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' data(iebudgets)
#' iebudgets2010 <- subset(iebudgets, year==2010)
#' wfm <- dfm(iebudgets2010, stopwords=TRUE)
#' wordcloudDfm(wfm, 1)  # plot the finance minister's speech as a wordcloud
wordcloudDfm <- function(dfm, doc.index, ...) {
    if (!is.dfm(dfm)) stop("word matrix argument must be a dfm object")
    wordcloud::wordcloud(features(dfm), dfm[doc.index, ], ...)
}


