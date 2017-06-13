#' plot semantic network based on collocations
#' 
#' Plot \link{fcm} as a network, where vertices are features and edges are cooccuraces 
#' using \code{\link[igraph]{igraph}}.
#' @param x a dfm object
#' @param width the maximum width of links that connects words
#' @param ignore a threadh for infrequentl collocations to be ignored
#' @examples
#' \dontrun{
#' toks <- tokens(corpus_subset(data_corpus_inaugural, President=="Obama"), remove_punct = TRUE)
#' toks <- tokens_remove(toks, feature = stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = 'window', tri = FALSE)
#' feats <- names(topfeatures(myfcm, 20))
#' textplot_network(myfcm[feats,feats], ignore = 0.5)
#' }
#' @export
#' @keywords plot
textplot_network <- function(x, width = 10, ignore = 0.5, ...) {
    
    if (!is.fcm(x))
        stop("x must be a fcm object")
    
    diag(x) <- 0
    suppressMessages(
    x[x < quantile(as.vector(x), ignore)] <- 0
    )
    temp <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE, diag = FALSE, mode = 'undirected')
    igraph::igraph_options(plot.layout = igraph::layout_with_fr, vertex.label.family = 'sans', 
                           vertex.label.color = 'black',
                           edge.color = adjustcolor('sky blue', 0.5),
                           vertex.color = adjustcolor('white', 1.0),
                           vertex.frame.color = adjustcolor('sky blue', 1.0),
                           edge.curved = 0.3)
    plot.igraph(temp, edge.width = igraph::E(temp)$weight / max(igraph::E(temp)$weight) * width, ...)
}


