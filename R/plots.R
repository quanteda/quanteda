#' plot features as a wordcloud
#' 
#' The default plot method for a \code{\link{dfm}} object.  Produces a wordcloud
#' plot for the features of the dfm, weighted by the total frequencies.  To 
#' produce word cloud plots for specific documents, the only way currently to do
#' this is to produce a dfm only from the documents whose features you want 
#' plotted.
#' @param x a dfm object
#' @param comparison if TRUE, plot a \link[wordcloud]{comparison.cloud} 
#'   comparison cloud instead of a simple wordcloud
#' @param ... additional parameters passed to to \link[wordcloud]{wordcloud} or 
#'   to \link{text} (and \link{strheight}, \link{strwidth})
#' @seealso \link[wordcloud]{wordcloud}
#' @examples
#' # plot the features (without stopwords) from Obama's two inaugural addresses
#' mydfm <- dfm(subset(inaugCorpus, President=="Obama"), verbose=FALSE,
#'              ignoredFeatures=stopwords("english"))
#' plot(mydfm)
#' 
#' # plot only Lincoln's inaugural address
#' plot(dfm(subset(inaugCorpus, President=="Lincoln"), verbose=FALSE,
#'      ignoredFeatures=stopwords("english")))
#' #comparison plot of Irish government vs opposition
#' side <- ifelse(docvars(ie2010Corpus, 'party') == 'FF' | docvars(ie2010Corpus, 'party') == 'FF', "govt", "opp")
#' docvars(ie2010Corpus, 'side') <- side
#' sideDfm <- weight(dfm(ie2010Corpus, groups = 'side'), type='tfidf')
#' plot(sideDfm, comparison=TRUE)
#' # plot in colors with some additional options passed to wordcloud
#' plot(mydfm, random.color=TRUE, rot.per=.25, colors=sample(colors()[2:128], 5))
#' @export
plot.dfm <- function(x, comparison=FALSE, ...) {
    if(comparison){
        if(ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud::comparison.cloud(t(x))
    }else{
        wordcloud::wordcloud(features(x), colSums(x), ...)
    }
}


#' plot a dispersion plot of key word(s)
#' 
#' Plots a dispersion or "x-ray" plot of a selected word pattern across a text.
#' @param x a \link{kwic} class object
#' @param ... additional arguments passed to \code{\link{plot}}
#' @note Currently works only for a a single document
#' @export
plot.kwic <- function(x, ...) {
    pos <- integer(attr(x, "ntoken"))
    pos[x$position] <- 1L
    plot(pos, xlab="Token Index", ylab = attr(x, "keyword"), type="h", 
         ylim=c(0,1), yaxt="n", col = "grey30", ...)
}


