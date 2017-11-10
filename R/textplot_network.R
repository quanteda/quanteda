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
#' toks <- corpus_subset(data_corpus_irishbudget2010) %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_remove(stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = "window", tri = FALSE)
#' feat <- names(topfeatures(myfcm, 30))
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(ignore = 0.5)
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(ignore = 0.8)
#' }
#' @export
#' @seealso \code{\link{fcm}}
#' @keywords textplot
textplot_network <- function(x, color = 'sky blue', ignore = 0.5, omit_isolated = TRUE, ...) {
    
    if (!is.fcm(x))
        stop("x must be a fcm object")
    
    x <- as.matrix(x)
    x[lower.tri(x, diag = FALSE)] <- 0
    
    # drop weak ties
    if (ignore > 0) {
        x[x < quantile(as.vector(x[x > 0]), ignore)] <- 0
    }
    
    # drop isolated words 
    if (omit_isolated) {
        f <- rowSums(x)
        x <- x[f > 0,f > 0]
    }
    
    n <- as.network(x, matrix.type = 'adjacency', directed = FALSE, ignore.eval = FALSE, names.eval = 'weight')
    network.vertex.names(n) <- colnames(x)
    ggplot(n, aes(x = x, y = y, xend = xend, yend = yend), arrow.gap = 0.00) +
           geom_edges(color = adjustcolor(color, 0.5), curvature = 0.2, alpha = 0.3, aes(size = weight), 
                      angle = 90, show.legend = FALSE) +
           theme_blank() +
           geom_nodelabel(aes(label = vertex.names), label.size = 0.05) +
           theme_blank()
}

