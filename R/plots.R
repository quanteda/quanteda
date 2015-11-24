#' plot features as a wordcloud
#' 
#' The default plot method for a \code{\link{dfm}} object.  Produces a wordcloud
#' plot for the features of the dfm, where the feature labels are plotted with 
#' their sizes proportional to their numerical values in the dfm.  When 
#' \code{comparison = TRUE}, it plots comparison word clouds by document.
#' @details The default is to plot the word cloud of all of the features in the 
#'   dfm, summed across documents.  To produce word cloud plots for specific 
#'   document or set of documents, you need to slice out the document(s) from
#'   the dfm.
#'   
#'   Comparison word cloud plots may be plotted by setting \code{comparison = 
#'   TRUE}, which plots a separate grouping for \emph{each document} in the dfm.
#'   This means that you will need to slice out just a few documents from the
#'   dfm, or to create a dfm where the "documents" represent a subset or a
#'   grouping of documents by some document variable.
#' @param x a dfm object
#' @param comparison if \code{TRUE}, plot a
#'   \code{\link[wordcloud]{comparison.cloud}} instead of a simple wordcloud,
#'   one grouping per document
#' @param ... additional parameters passed to to \link[wordcloud]{wordcloud} or 
#'   to \link{text} (and \link{strheight}, \link{strwidth})
#' @seealso \code{\link[wordcloud]{wordcloud}},
#'   \code{\link[wordcloud]{comparison.cloud}}
#' @examples
#' # plot the features (without stopwords) from Obama's two inaugural addresses
#' mydfm <- dfm(subset(inaugCorpus, President=="Obama"), verbose = FALSE,
#'              ignoredFeatures = stopwords("english"))
#' plot(mydfm)
#' 
#' # plot in colors with some additional options passed to wordcloud
#' plot(mydfm, random.color = TRUE, rot.per = .25, colors = sample(colors()[2:128], 5))
#'
#' \dontrun{# comparison plot of Irish government vs opposition
#' govtopp <- factor(ifelse(ie2010Corpus[, "party"] %in% c("FF", "Green"), "Govt", "Opp"))
#' govtoppDfm <- dfm(ie2010Corpus, groups = govtopp, verbose = FALSE)
#' plot(tfidf(govtoppDfm), comparison = TRUE)
#' # compare to non-tf-idf version
#' plot(sideDfm, comparison = TRUE)}
#' @export
plot.dfm <- function(x, comparison = FALSE, ...) {
    if (comparison) {
        if (ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud::comparison.cloud(t(x))
    } else {
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


