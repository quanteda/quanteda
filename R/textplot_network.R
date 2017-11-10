#' plot a semantic network of feature co-occurrences
#' 
#' Plot an \link{fcm} object as a network, where vertices are features and edges
#' are co-occurrences, using \code{\link[igraph]{igraph}}.
#' @param x a \link{dfm} object
#' @param width the maximum width of links that connects words
#' @param size size of the vertices
#' @param shape shape of the vertices
#' @param ignore a thread for infrequent collocations to be ignored
#' @param ... additional parameters passed to \code{\link[igraph]{plot.igraph}}.
#'   Note that the 'vertex.' prefix needs to be added.
#' @author Kohei Watanabe and Stefan Müller
#' @examples
#' \dontrun{
#' toks <- corpus_subset(data_corpus_inaugural, President == "Obama") %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_remove(stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = "window", tri = FALSE)
#' feat <- names(topfeatures(myfcm, 20))
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network()
#' fcm_select(myfcm, feat, verbose = FALSE) %>% 
#'     textplot_network(size = 20, shape = "vrectangle")
#' }
#' @export
#' @seealso \code{\link{fcm}}
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
    igraph::plot.igraph(temp, edge.width = igraph::E(temp)$weight / max(igraph::E(temp)$weight) * width, ...)
}

