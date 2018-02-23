#' Plot word keyness
#' 
#' Plot the results of a "keyword" of features comparing their differential 
#' associations with a target and a reference group, after calculating keyness
#' using \code{\link{textstat_keyness}}.
#' @param x a return object from \code{\link{textstat_keyness}}
#' @param show_reference logical; if \code{TRUE}, show key reference features in
#'   addition to key target features
#' @param show_legend logical; if \code{TRUE}, show legend
#' @param n integer; number of features to plot
#' @param min_count numeric; minimum total count of feature across the target 
#'   and reference categories, for a feature to be included in the plot
#' @param margin numeric; size of margin where feature labels are shown 
#' @param color character; colors of bars for target and reference documents.
#'   \code{color} must have two elements when \code{show_reference = TRUE}.
#' @param labelcolor character; color of feature labels.
#' @param labelsize numeric; size of feature labels and bars.
#' @param font character; font-family of texts. Use default font if \code{NULL}.
#' @return a \pkg{ggplot2} object
#' @export
#' @author Haiyan Wang
#' @seealso \code{\link{textstat_keyness}}
#' @keywords textplot
#' @examples
#' 
#' # compare Trump v. all other speeches
#' pres_corp <- data_corpus_inaugural
#' pres_dfm <- dfm(pres_corp, groups = "President", remove = stopwords("english"),
#'                 remove_punct = TRUE)
#' pres_key <- textstat_keyness(pres_dfm, target = "Trump")               
#' textplot_keyness(pres_key)
#' textplot_keyness(pres_key, show_reference = FALSE) 
#' 
#' # compare Trump v. Obama speeches by PMI
#' to_corp <- corpus_subset(data_corpus_inaugural, 
#'                          President %in% c("Obama", "Trump"))
#' to_dfm <- dfm(to_corp, groups = "President", remove = stopwords("english"),
#'               remove_punct = TRUE)
#' to_key <- textstat_keyness(to_dfm, target = "Trump", measure = 'pmi')
#' 
#' textplot_keyness(to_key, margin = 0.2)
#' textplot_keyness(to_key, show_reference = FALSE, margin = 0.2) 
#' 
textplot_keyness <-  function(x, show_reference = TRUE, show_legend = TRUE, 
                              n = 20L, min_count = 2L, margin = 0.05,
                              color = c("#1F78B4", "#A6CEE3"), labelcolor = '#4D4D4D',
                              labelsize = 4, font = NULL) {
    UseMethod("textplot_keyness")
}

#' @export
textplot_keyness.default <- function(x, show_reference = TRUE, show_legend = TRUE, 
                                     n = 20L, min_count = 2L, margin = 0.05,
                                     color = c("#1F78B4", "#A6CEE3"), labelcolor = '#4D4D4D',
                                     labelsize = 4, font = NULL) {
    stop(friendly_class_undefined_message(class(x), "textplot_keyness"))
}

#' @import ggplot2
#' @export
textplot_keyness.keyness <- function(x, show_reference = TRUE, show_legend = TRUE,
                                     n = 20L, min_count = 2L, margin = 0.05,
                                     color = c("#1F78B4", "#A6CEE3"), labelcolor = '#4D4D4D',
                                     labelsize = 4, font = NULL) {
    
    
    font <- check_font(font)
    if (show_reference)
        if (length(color) != 2)
            stop('color must have two values when show_reference is TRUE.')
    
    # extract attribute befor subsetting
    docname <- attr(x, "documents")
    measure <- colnames(x)[2]
    
    # drop infrequent words
    data <- x[(x$n_target + x$n_reference) >= min_count,,drop = FALSE]
    
    if (nrow(data) < 1)
        stop ("Too few words in the documents.")
    
    data$keyness <- data[[2]]
    data$right <- data$keyness >= 0
    if (show_reference) {
        t <- intersect(which(data$right), head(seq(nrow(data)), n))
        r <- intersect(which(!data$right), tail(seq(nrow(data)), n))
        i <- union(t, r)
    } else {
        i <- intersect(which(data$right), head(seq(nrow(data)), n))
    }
    data <- data[i,,drop = FALSE]
    #data$width <- stri_width(data$feature)
    if (show_reference) {
        data$color <- color[2 - data$right]
    } else {
        data$color <- color[1]
    }
    
    if (length(docname) < 2) {
        docname <- c("Target", "Reference")
    } else if (length(docname) > 2) {
        docname <- c(docname[1], "Reference")
    }

    data$x1 <- ifelse(data$right, abs(data$keyness), abs(data$keyness) * -1)
    data$y1 <- rank(data$keyness, ties.method = "first")
    data$x2 <- 0
    data$y2 <- data$y
    margin <- margin * max(abs(data$x1)) * 2
    
    x1 <- y1 <- x2 <- y2 <- feature <- NULL
    ggplot(data) +  
         xlim(if (show_reference) min(data$x1) - margin else 0, max(data$x1) + margin) + 
         geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = color), 
                      size = labelsize) +
         scale_colour_identity(NULL, labels = docname, 
                              guide = if (show_legend) 'legend' else FALSE) + 
         xlab(measure) +
         geom_label(aes(x = x1, y = y1, label = feature), label.size = 0, fill = NA,
                    vjust = 'center', hjust = ifelse(data$right, 'left', 'right'),
                    color = labelcolor, size = labelsize, family = font) +
         theme_bw() +
         theme(axis.line = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(linetype = "dotted"))
} 
