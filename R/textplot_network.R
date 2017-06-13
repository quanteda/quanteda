#' plot semantic network based on collocations
#' 
#' Plot \link{fcm} as a network, where vertices are features and edges are cooccuraces 
#' using \code{\link[igraph]{igraph}}.
#' @param x a dfm object
#' @param width the maximum width of links that connects words (default is 10)
#' @param size size of the vertex (default is 15)
#' @param shape shape of the vertex (default is "circle")
#' @param ignore a thread for infrequent collocations to be ignored
#' @param ... additional parameters passed to \code{\link[igraph]{plot.igraph}}. Note that the 'vertex.' prefix needs to be added.
#' @examples
#' \dontrun{
#' toks <- tokens(corpus_subset(data_corpus_inaugural, President=="Obama"), remove_punct = TRUE)
#' toks <- tokens_remove(toks, feature = stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = 'window', tri = FALSE)
#' feats <- names(topfeatures(myfcm, 20))
#' textplot_network(myfcm[feats, feats], ignore = 0.5)
#' textplot_network(myfcm[feats, feats], ignore = 0.5, size = 20, shape = "vrectangle")
#' }
#' @export
#' author Kohei Watanabe and Stefan Müller
#' @seealso \code{\link{textmodel_wordfish}}, \code{\link{textmodel_wordscores}}, 
#'   \code{\link{coef.textmodel}}
#' @keywords textplot
textplot_network <- function(x, width = 10, size = 15, shape = "circle", ignore = 0.5, ...) {
  
  if (!is.fcm(x))
    stop("x must be a fcm object")
  
  diag(x) <- 0
  suppressMessages(
    x[x < quantile(as.vector(x), ignore)] <- 0
  )
  temp <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE, diag = FALSE, mode = 'undirected')
  igraph::igraph_options(plot.layout = igraph::layout_with_fr, vertex.label.family = 'sans', 
                         vertex.label.color = 'black',
                         vertex.size = size,
                         vertex.shape = shape,
                         edge.color = adjustcolor('sky blue', 0.5),
                         vertex.color = adjustcolor('white', 1.0),
                         vertex.frame.color = adjustcolor('sky blue', 1.0),
                         edge.curved = 0.3)
  plot.igraph(temp, edge.width = igraph::E(temp)$weight / max(igraph::E(temp)$weight) * width, ...)
}

