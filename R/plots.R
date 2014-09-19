#' plot features as a wordcloud
#' 
#' The default plot method for a \code{\link{dfm}} object.  Produces a wordcloud
#' plot for the features of the dfm, weighted by the total frequencies.  To 
#' produce word cloud plots for specific documents, the only way currently to do
#' this is to produce a dfm only from the documents whose features you want 
#' plotted.
#' @param x a dfm object
#' @param ... additional parameters to \link[wordcloud]{wordcloud} or to \link{text}
#'   (and \link{strheight}, \link{strwidth})
#' @seealso \link[wordcloud]{wordcloud}
#' @examples
#' # plot the features (without stopwords) from Obama's two inaugural addresses
#' mydfm <- dfm(subset(inaugCorpus, President=="Obama"), verbose=FALSE, stopwords=TRUE)
#' plot(mydfm)
#' 
#' # plot only Lincoln's inaugural address
#' plot(dfm(subset(inaugCorpus, President=="Lincoln"), verbose=FALSE, stopwords=TRUE))
#' 
#' # plot in colors with some additional options passed to wordcloud
#' plot(mydfm, random.color=TRUE, rot.per=.25, colors=sample(colors()[2:128], 5))
#' @export
plot.dfm <- function(x, ...) {
    wordcloud::wordcloud(features(x), colSums(x), ...)
}





