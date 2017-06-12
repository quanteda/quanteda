#' plot semantic network based on collocations
#' @param x a dfm object
#' @param width the maximum width of links that connects words
#' @param ignore a threadh for infrequentl collocations to be ignored
#' @examples
#' \dontrun{
#' # plot the features (without stopwords) from Obama's two inaugural addresses
#' mydfm <- dfm(corpus_subset(data_corpus_inaugural, President=="Obama"), verbose = FALSE,
#'              remove = stopwords("english"))
#' textplot_network(mydfm, ignore = 0.999)
#' }
#' @export
#' @keywords plot
textplot_network <- function(x, width = 10, ignore = 0.99, ...) {
    
    if (!is.dfm(x) & !is.tokens(x))
        stop("x must be a dfm or tokens object")
    
    if (is.tokens(x))
        x <- dfm(x, verbose = FALSE)
    
    if (is.dfm(x))
        x <- fcm(x, tri = FALSE, verbose = FALSE)
    
    diag(x) <- 0 
    suppressMessages(
    x[x < quantile(as.vector(x), ignore)] <- 0
    )
    temp <- graph_from_adjacency_matrix(x, weighted = TRUE, diag = FALSE, mode = 'undirected')
    igraph.options(plot.layout = layout_with_fr, vertex.label.family = 'sanserif', 
                   vertex.label.color = 'black',
                   edge.color = adjustcolor('sky blue', 0.5),
                   vertex.color = adjustcolor('white', 1.0),
                   vertex.frame.color = adjustcolor('sky blue', 1.0),
                   edge.curved = 0.3)
    plot(temp, edge.width = E(temp)$weight / max(E(temp)$weight) * width, ...)
}


