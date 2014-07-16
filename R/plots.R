#' Plot a word cloud for a \link{dfm}
#'
#' plots a document as a wordcloud of its features
#' 
#' @param dfm document-feature matrix created in quanteda
#' @param document index of the document whose words will be plotted
#' @param ... additional arguments to pass to \link[wordcloud]{wordcloud} 
#' @return None
#' @rdname wordcloud
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' data(iebudgets)
#' iebudgets2010 <- subset(iebudgets, year==2010)
#' wfm <- dfm(iebudgets2010, stopwords=TRUE)
#' wordcloudDfm(wfm, 1)  # plot the finance minister's speech as a wordcloud
wordcloudDfm <- function(dfm, doc.index, ...) {
    if (!is.wfm(dfm)) stop("word matrix argument must be a dfm object")
    require(wordcloud)
    wordcloud::wordcloud(words(dfm), dfm[doc.index, ], ...)
}
