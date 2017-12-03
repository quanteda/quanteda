#' plot a network of feature co-occurrences
#'
#' Plot an \link{fcm} object as a network, where edges show co-occurrences of
#' features.
#' @param x a \link{fcm} or \link{dfm}  object
#' @param min_freq a percentail or frequency count threashold for cooccurances
#'   of features to be plotted.
#' @param omit_isolated if \code{TRUE}, features do not occuare more frequent
#'   than \code{min_freq} will be omitted from the plot
#' @param edge_color color of edges that connect vertices.
#' @param edge_alpha opacity of edges ranging from 0 to 1.0.
#' @param edge_size size of edges for most frequent cooccurances. The size of
#'   other edges are determined proportionally to the highest frequency.
#' @param vertex_size size of vertices.
#' @param vertex_color color of vertices.
#' @param text_color color of texts. Default to the same as \code{vertex_color}.
#'   If \code{NA} is given, texts are not rendered.
#' @param offset if \code{NULL}, the distance between vertices and texts are
#'   determined automatically.
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
#' fcm_select(myfcm, feat, verbose = FALSE) %>%
#'         textplot_network(min_freq = 0.8, text_color = rep(c('gray40', NA), 15))
#' }
#' @export
#' @seealso \code{\link{fcm}}
#' @import network ggplot2 ggrepel
#' @keywords textplot
textplot_network <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                             edge_color = 'skyblue', edge_alpha = 0.5, edge_size = 2, 
                             vertex_color = 'gray40', vertex_size = 2,
                             text_color = NULL,
                             offset = NULL) {
    UseMethod("textplot_network")
}

#' @rdname textplot_network
#' @noRd
#' @export
textplot_network.dfm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 edge_color = 'skyblue', edge_alpha = 0.5, edge_size = 2, 
                                 vertex_color = 'gray40', vertex_size = 2,
                                 text_color = NULL,
                                 offset = NULL) {

    textplot_network(fcm(x), min_freq = min_freq, omit_isolated = omit_isolated, 
                     edge_color = edge_color, edge_alpha = edge_alpha, edge_size = edge_size, 
                     vertex_color = vertex_color, vertex_size = vertex_size,
                     text_color = text_color,
                     offset = NULL)
}

    
#' @rdname textplot_network
#' @noRd
#' @export
textplot_network.fcm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 edge_color = 'skyblue', edge_alpha = 0.5, edge_size = 2, 
                                 vertex_color = 'gray40', vertex_size = 2,
                                 text_color = NULL,
                                 offset = NULL) {

    x <- as(x, 'dgTMatrix')
    
    if (is.null(text_color))
        text_color <- vertex_color
    
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
        if (length(text_color) > 1) text_color <- text_color[i]
        if (length(vertex_color) > 1) vertex_color <- vertex_color[i]
    }
    
    if (nrow(x) > 1000)
        stop('fcm is too large for a network plot')
    
    if (all(x@x == 0))
        stop('There is no coocurance higher than the threashold')
    
    n <- network(as.matrix(x), matrix.type = 'adjacency', directed = FALSE, 
                 ignore.eval = FALSE, names.eval = 'weight')
    vertex <- data.frame(sna::gplot.layout.fruchtermanreingold(n, NULL))
    colnames(vertex) <- c('x', 'y')
    vertex$label <- colnames(x)

    weight <- get.edge.attribute(n, "weight")
    weight <- weight / max(weight)
    
    index <- as.edgelist(n)
    edge <- data.frame(x1 = vertex[,1][index[,1]], 
                       y1 = vertex[,2][index[,1]],
                       x2 = vertex[,1][index[,2]], 
                       y2 = vertex[,2][index[,2]],
                       weight = weight[index[,1]])
    
    plot <- ggplot() + 
        geom_curve(data = edge, aes(x = x1, y = y1, xend = x2, yend = y2), 
                   color = edge_color, curvature = 0.2, alpha = edge_alpha, lineend = "round",
                   angle = 90, size = weight * edge_size) + 
        geom_point(data = vertex, aes(x, y), color = vertex_color, size = vertex_size, shape = 19)
    
        if (is.null(offset)) {
            plot <- plot + geom_text_repel(data = vertex, aes(x, y, label = label),
                                           segment.color = vertex_color, segment.size = 0.2,
                                           color = text_color)
        } else {
            plot <- plot + geom_text(data = vertex, aes(x, y, label = label),
                                     nudge_y = offset, color = text_color)
        }

    plot <- plot + 
        scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
        theme(panel.background = element_blank()) + 
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
        theme(legend.background = element_rect(color = NA)) + 
        theme(panel.background = element_rect(fill = "white", color = NA)) +
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    
    return(plot)

}


