#' Plot a network of feature co-occurrences
#'
#' Plot an \link{fcm} object as a network, where edges show co-occurrences of
#' features.
#' @param x a \link{fcm} or \link{dfm}  object
#' @param min_freq a frequency count threshold or proportion for co-occurrence
#'   frequencies of features to be plotted.
#' @param omit_isolated if \code{TRUE}, features do not occur more frequent than
#'   \code{min_freq} will be omitted from the plot
#' @param edge_color color of edges that connect vertices.
#' @param edge_alpha opacity of edges ranging from 0 to 1.0.
#' @param edge_size size of edges for most frequent co-occurrence The size of
#'   other edges are determined proportionally to the highest frequency.
#' @param vertex_size size of vertices.
#' @param vertex_color color of vertices.
#' @param vertex_labelcolor color of texts. Defaults to the same as
#'   \code{vertex_color}. If \code{NA} is given, texts are not rendered.
#' @param offset if \code{NULL}, the distance between vertices and texts are
#'   determined automatically.
#' @param vertex_labelfont font-family of texts. Use default font if \code{NULL}.
#' @param ... additional arguments passed to \link[network]{network}.
#' @details Currently the size of the network is limited to 1000, because of the
#'   computationally intensive nature of network formation for larger matrices.
#'   When the \link{fcm} is large, users should select features using
#'   \link{fcm_select}, set the threshold using \code{min_freq}, or implement
#'   own plotting function using \code{\link[=as.network.fcm]{as.network}}.
#' @author Kohei Watanabe and Stefan Müller
#' @examples
#' toks <- corpus_subset(data_corpus_irishbudget2010) %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_remove(stopwords("english"), padding = FALSE)
#' myfcm <- fcm(toks, context = "window", tri = FALSE)
#' feat <- names(topfeatures(myfcm, 30))
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(min_freq = 0.5)
#' fcm_select(myfcm, feat, verbose = FALSE) %>% textplot_network(min_freq = 0.8)
#' fcm_select(myfcm, feat, verbose = FALSE) %>%
#'         textplot_network(min_freq = 0.8, vertex_labelcolor = rep(c('gray40', NA), 15))
#' @export
#' @seealso \code{\link{fcm}}
#' @import ggplot2 
#' @keywords textplot
textplot_network <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                             edge_color = '#1F78B4', edge_alpha = 0.5, 
                             edge_size = 2, 
                             vertex_color = '#4D4D4D', vertex_size = 2,
                             vertex_labelcolor = NULL,
                             offset = NULL, 
                             vertex_labelfont = NULL, ...) {
    UseMethod("textplot_network")
}

#' @export
textplot_network.dfm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 edge_color = '#1F78B4', edge_alpha = 0.5, 
                                 edge_size = 2, 
                                 vertex_color = '#4D4D4D', vertex_size = 2,
                                 vertex_labelcolor = NULL,
                                 offset = NULL, 
                                 vertex_labelfont = NULL, ...) {

    textplot_network(fcm(x), min_freq = min_freq, omit_isolated = omit_isolated, 
                     edge_color = edge_color, edge_alpha = edge_alpha, 
                     edge_size = edge_size, 
                     vertex_color = vertex_color, vertex_size = vertex_size,
                     vertex_labelcolor = vertex_labelcolor,
                     offset = NULL, 
                     vertex_labelfont = vertex_labelfont, ...)
}

    
#' @export
textplot_network.fcm <- function(x, min_freq = 0.5, omit_isolated = TRUE, 
                                 edge_color = '#1F78B4', edge_alpha = 0.5, 
                                 edge_size = 2, 
                                 vertex_color = '#4D4D4D', vertex_size = 2,
                                 vertex_labelcolor = NULL,
                                 offset = NULL, 
                                 vertex_labelfont = NULL, ...) {
    
    label <- x1 <- x2 <- y <- y1 <- y2 <- NULL
    
    vertex_labelfont <- check_font(vertex_labelfont)
    n <- as.network(x, min_freq = min_freq, omit_isolated = omit_isolated, ...)

    vertex <- data.frame(sna::gplot.layout.fruchtermanreingold(n, NULL))
    colnames(vertex) <- c('x', 'y')
    vertex$label <- network::network.vertex.names(n)

    weight <- network::get.edge.attribute(n, "weight")
    weight <- weight / max(weight)
    
    index <- network::as.edgelist(n)
    edge <- data.frame(x1 = vertex[,1][index[,1]], 
                       y1 = vertex[,2][index[,1]],
                       x2 = vertex[,1][index[,2]], 
                       y2 = vertex[,2][index[,2]],
                       weight = weight[index[,1]])
    
    if (is.null(vertex_labelcolor))
        vertex_labelcolor <- vertex_color
    
    # drop colors of omitted vertices
    l <- featnames(x) %in% network::network.vertex.names(n)
    if (length(vertex_labelcolor) > 1) vertex_labelcolor <- vertex_labelcolor[l]
    if (length(vertex_color) > 1) vertex_color <- vertex_color[l]
    if (length(vertex_size) > 1) vertex_size <- vertex_size[l]
    
    plot <- ggplot() + 
        geom_curve(data = edge, aes(x = x1, y = y1, xend = x2, yend = y2), 
                   color = edge_color, curvature = 0.2, 
                   alpha = edge_alpha, lineend = "round",
                   angle = 90, size = weight * edge_size) + 
        geom_point(data = vertex, aes(x, y), color = vertex_color, 
                   size = vertex_size, shape = 19)
    
        if (is.null(offset)) {
            plot <- plot + 
                ggrepel::geom_text_repel(data = vertex, 
                                         aes(x, y, label = label),
                                         segment.color = vertex_color, 
                                         segment.size = 0.2,
                                         color = vertex_labelcolor,
                                         family = vertex_labelfont)
        } else {
            plot <- plot + 
                geom_text(data = vertex, aes(x, y, label = label),
                          nudge_y = offset, 
                          color = vertex_labelcolor,
                          family = vertex_labelfont)
        }

    plot <- plot + 
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank())
    
    return(plot)
}

# as.network ----------

#' redefinition of network::as.network()
#' @param x input object
#' @param ... additional arguments
#' @keywords internal
#' @export 
as.network <- function(x, ...) {
    UseMethod("as.network")
}

#' @export
as.network.default <- function(x, ...) {
    network::as.network(x, ...)
}

#' @rdname textplot_network
#' @method as.network fcm
#' @export
as.network.fcm <- function(x, min_freq = 0.5, omit_isolated = TRUE, ...) {

    if (nfeat(x) > 1000) stop('fcm is too large for a network plot')

    x <- as(x, 'dgTMatrix')
    
    # drop weak edges
    if (min_freq > 0) {
        if (min_freq < 1) {
            min_freq <- quantile(x@x, min_freq)
        }
        l <- x@x >= min_freq
        x@i <- x@i[l]
        x@j <- x@j[l]
        x@x <- x@x[l]
    }
    
    # drop isolated vertices 
    if (omit_isolated) {
        i <- which(rowSums(x) > 0)
        x <- x[i, i]
    }
    
    if (all(x@x == 0))
        stop('There is no co-occurence higher than the threshold')
    
    network::network(as.matrix(x), matrix.type = 'adjacency', directed = FALSE, 
            ignore.eval = FALSE, names.eval = 'weight', ...)
}

#' summary.character method to override the network::summary.character()
#' 
#' Necessary to prevent the \pkg{network} package's \code{summary.character} method 
#' from causing inconsistent behaviour with other summary methods.
#' @param object the character input
#' @param ... for additional passing of arguments to default method
#' @keywords internal
#' @method summary character
#' @export
summary.character <- function(object, ...) {
    base::summary.default(object, ...)
}
