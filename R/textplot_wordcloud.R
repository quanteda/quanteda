#' plot features as a wordcloud
#' 
#' Plot a \link{dfm} or \link{tokens} object as a wordcloud, where the feature 
#' labels are plotted with their sizes proportional to their numerical values in
#' the dfm.  When \code{comparison = TRUE}, it plots comparison word clouds by 
#' document.
#' @details The default is to plot the word cloud of all features, summed across
#'   documents.  To produce word cloud plots for specific document or set of 
#'   documents, you need to slice out the document(s) from the dfm or tokens
#'   object.
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
#' mydfm <- dfm(corpus_subset(data_corpus_inaugural, President=="Obama"), verbose = FALSE,
#'              remove = stopwords("english"))
#' textplot_wordcloud(mydfm)
#' 
#' # plot in colors with some additional options passed to wordcloud
#' textplot_wordcloud(mydfm, random.color = TRUE, rot.per = .25, 
#'                    colors = sample(colors()[2:128], 5))
#' 
#' \dontrun{
#' # comparison plot of Irish government vs opposition
#' docvars(data_corpus_irishbudget2010, "govtopp") <- 
#'     factor(ifelse(data_corpus_irishbudget2010[, "party"] %in% c("FF", "Green"), "Govt", "Opp"))
#' govtoppDfm <- dfm(data_corpus_irishbudget2010, groups = "govtopp", verbose = FALSE)
#' textplot_wordcloud(tfidf(govtoppDfm), comparison = TRUE)
#' # compare to non-tf-idf version
#' textplot_wordcloud(govtoppDfm, comparison = TRUE)
#' }
#' @export
#' @keywords plot
textplot_wordcloud <- function(x, comparison = FALSE, ...) {
    
    if (!is.dfm(x) & !is.tokens(x))
        stop("x must be a dfm or tokens object")
    
    if (is.tokens(x))
        x <- dfm(x, verbose = FALSE)
    
    if (comparison) {
        if (ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud::comparison.cloud(t(as.matrix(x)), ...)
    } else {
        wordcloud::wordcloud(featnames(x), colSums(x), ...)
    }
}


