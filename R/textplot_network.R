#' plot a semantic network of feature co-occurrences
#'
#' Plot an \link{fcm} object as a network, where vertices are features and edges
#' are co-occurrences.
#' @param x a \link{fcm} or \link{dfm}  object
#' @param min_freq a percentail or frequency count threashold for cooccurances
#'   of features to be plotted.
#' @param omit_isolated if \code{TRUE}, features do not occuare more frequent
#'   than \code{min_freq} will be omitted from the plot
#' @param col color of edges
#' @param alpha opacity of edges
#' @param size size of vertices
#' @param width width of edges
#' @param offset if \code{NULL}, distances between vertices and labels are
#'   determined automatically as far as the number of vertices is less than 100.
#' @author Kohei Watanabe and Stefan Müller
#' @examples
#' \dontrun{
#' toks <- corpus_subset(data_corpus_irishbudget2010) %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_remove(stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = "window", tri = FALSE)
#' feat <- names(topfeatures(myfcm, 30))
#' textplot_network(myfcm, min_freq = 10)
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(min_freq = 0.5)
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(min_freq = 0.8)
#' }
#' @export
#' @seealso \code{\link{fcm}}
#' @import network ggplot2 ggrepel
#' @keywords textplot
textplot_network <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                             col = 'skyblue', alpha = 0.5, size = 2, width = 2, 
                             offset = NULL) {
    UseMethod("textplot_network")
}

#' @rdname textplot_network
#' @noRd
#' @export
textplot_network.dfm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 col = 'skyblue', alpha = 0.5, size = 2, width = 2, 
                                 offset = NULL) {

    textplot_network(fcm(x), min_freq, omit_isolated, col, alpha, size, width, offset)
}

    
#' @rdname textplot_network
#' @noRd
#' @export
textplot_network.fcm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 col = 'skyblue', alpha = 0.5, size = 2, width = 2, 
                                 offset = NULL) {

    x <- as(x, 'dgTMatrix')
    
    # drop weak ties
    if (min_freq > 0) {
        if (min_freq < 1) {
            min_freq <- quantile(x@x, min_freq)
        }
        l <- x@x >= min_freq
        x@i <- x@i[l]
        x@j <- x@j[l]
        x@x <- x@x[l]
    }
    
    # drop isolated words 
    if (omit_isolated) {
        i <- which(rowSums(x) > 0)
        x <- x[i, i]
    }
    
    if (nrow(x) > 1000)
        stop('fcm is too large for a network plot')
    
    if (all(x@x == 0))
        stop('There is no coocurance higher than the threashold')
    
    n <- network(as.matrix(x), matrix.type = 'adjacency', directed = FALSE, 
                 ignore.eval = FALSE, names.eval = 'weight')
    vertex <- data.frame(sna::gplot.layout.fruchtermanreingold(n, NULL))
    vertex$label <- colnames(x)
    
    weight <- get.edge.attribute(n, "weight")
    weight <- weight / max(weight)
    
    index <- as.edgelist(n)
    edge <- data.frame(X1 = vertex[,1][index[,1]], 
                       Y1 = vertex[,2][index[,1]],
                       X2 = vertex[,1][index[,2]], 
                       Y2 = vertex[,2][index[,2]],
                       weight = weight[index[,1]])
    
    plot <- ggplot() + 
        geom_curve(data = edge, aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                   colour = col,  curvature = 0.2, alpha = alpha, lineend = "round",
                   angle = 90, size = weight * width) + 
        geom_point(data = vertex, aes(X1, X2), color = 'gray40', size = size, shape = 19)
    
        if (is.null(offset)) { 
            plot <- plot + geom_text_repel(data = vertex, aes(X1, X2, label = label), 
                                           segment.size = 0, color = 'gray40')
        } else {
            plot <- plot + geom_text(data = vertex, aes(X1, X2, label = label), 
                                     nudge_y = offset, color = 'gray40')
        }
    
    plot <- plot + 
        scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
        theme(panel.background = element_blank()) + 
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
        theme(legend.background = element_rect(colour = NA)) + 
        theme(panel.background = element_rect(fill = "white", colour = NA)) +
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    
    return(plot)

}


