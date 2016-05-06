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
#'   Comparison word cloud plots may be plotted by setting 
#'   \code{comparison = TRUE}, which plots a separate grouping for 
#'   \emph{each document} in the dfm.
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
#' \dontrun{
#' # comparison plot of Irish government vs opposition
#' docvars(ie2010Corpus, "govtopp") <- 
#'     factor(ifelse(ie2010Corpus[, "party"] %in% c("FF", "Green"), "Govt", "Opp"))
#' govtoppDfm <- dfm(ie2010Corpus, groups = "govtopp", verbose = FALSE)
#' plot(tfidf(govtoppDfm), comparison = TRUE)
#' # compare to non-tf-idf version
#' plot(govtoppDfm, comparison = TRUE)
#' }
#' @export
plot.dfm <- function(x, comparison = FALSE, ...) {
    if (comparison) {
        if (ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud::comparison.cloud(t(as.matrix(x)), ...)
    } else {
        wordcloud::wordcloud(features(x), colSums(x), ...)
    }
}


#' plot a dispersion plot of key word(s)
#' 
#' Plots a dispersion or "x-ray" plot of selected word pattern(s) across one or more texts.
#' The format of the plot depends on the number of \link{kwic} class objects passed: if there
#' is only one document, keywords are plotted one below the other. If there are multiple documents
#' they are plotted one below the other, with keywords shown side-by-side.
#' @param ... any number of \link{kwic} class objects
#' @import ggplot2
#' @import grid
#' @author Adam Obeng
#' @return \code{plot.kwic} returns a ggplot object
#' @examples 
#' \dontrun{
#' plot(kwic(inaugCorpus, "american"))
#' plot(kwic(inaugCorpus, "american"), kwic(inaugCorpus, "people"))
#' }
#' @export
plot.kwic <- function(...) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("You must have ggplot2 installed to make a dispersion plot.")
    if(!requireNamespace("grid", quietly = TRUE)) 
      stop("You must have grid installed to make a dispersion plot.")

   arguments <- list(...)
   x <- lapply(arguments, function(x) { x$keyword <- attr(x, 'keyword'); x})
   x <- do.call(rbind, x)

   plot <- ggplot2::ggplot(x, ggplot2::aes(x=position, y=1)) + ggplot2::geom_segment(ggplot2::aes(xend=position, yend=0)) + 
   ggplot2::theme(axis.line=ggplot2::element_blank(),
       panel.background=ggplot2::element_blank(),panel.grid.major.y=ggplot2::element_blank(),
       panel.grid.minor.y=ggplot2::element_blank(), plot.background=ggplot2::element_blank(),
       axis.ticks.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),
       panel.margin = grid::unit(0.1, "lines"), panel.border=ggplot2::element_rect(colour = "gray", fill=NA),
       strip.text.y=ggplot2::element_text(angle=0)
   ) 


   if ((length(unique(x$docname)) > 1)) {
      # If there is more than one document, put documents on the panel y-axis and keyword(s)
      # on the panel x-axis
      plot <- plot + ggplot2::facet_grid(docname~keyword) + 
      ggplot2::labs(x='Token index', y='Document', title = paste('Lexical dispersion plot'))
   }
   else {
      # If not, put keywords on the panel y-axis and the document name in the title
      plot <- plot + ggplot2::facet_grid(keyword~.) + 
      ggplot2::labs(x='Token index',
          y='', title = paste('Lexical dispersion plot, document:', x$docname[[1]]))
   }

   plot
}


