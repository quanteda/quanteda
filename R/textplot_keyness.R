#' Plot word keyness
#' 
#' Plot the results of a "keyword" of features comparing their differential 
#' associations with a target and a reference group, after calculating keyness
#' using \code{\link{textstat_keyness}}.
#' @param x a return object from \code{\link{textstat_keyness}}
#' @param show_reference logical; if \code{TRUE}, show key reference features in
#'   addition to key target features
#' @param n integer; number of features to plot
#' @param min_count numeric; minimum total count of feature across the target 
#'   and reference categories, for a feature to be included in the plot
#' @return a \pkg{ggplot2} object
#' @export
#' @author Haiyan Wang
#' @seealso \code{\link{textstat_keyness}}
#' @keywords textplot
#' @examples
#' \dontrun{
#' # compare Trump v. Obama speeches
#' prescorpus <- corpus_subset(data_corpus_inaugural, 
#'                             President %in% c("Obama", "Trump"))
#' presdfm <- dfm(prescorpus, groups = "President", remove = stopwords("english"),
#'                remove_punct = TRUE)
#' result <- textstat_keyness(presdfm, target = "Trump")
#' 
#' # plot estimated word keyness
#' textplot_keyness(result) 
#' textplot_keyness(result, show_reference = FALSE)
#' }
textplot_keyness <-  function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    UseMethod("textplot_keyness")
}

#' @export
textplot_keyness.default <- function(x, show_reference = TRUE, n = 20L, min_count = 2L,
                                     color = c("#A6CEE3", "#1F78B4"), font = NULL) {
    stop(friendly_class_undefined_message(class(x), "textplot_keyness"))
}

#' @import ggplot2
#' @export
textplot_keyness.keyness <- function(x, show_reference = TRUE, n = 20L, min_count = 2L,
                                     color = c("#A6CEE3", "#1F78B4"), font = NULL) {
    
    
    font <- check_font(font)
    
    # extract attribute befor subsetting
    docname <- attr(x, "documents")
    measure <- colnames(x)[2]
    
    # drop infrequent words
    data <- x[(x$n_target + x$n_reference) >= min_count,,drop = FALSE]
    
    if (nrow(data) < 1) {
        stop ("Too few words in the documents.")
    }
    
    
    data$keyness <- data[[2]]
    if (measure == "pmi") {
        limit <- 1
    } else {
        limit <- 0
    }
    data$right <- data$keyness >= limit
    if (show_reference) {
        t <- intersect(which(data$right), head(seq(nrow(data)), n / 2))
        r <- intersect(which(!data$right), head(seq(nrow(data), n / 2)))
        i <- union(t, r)
    } else {
        i <- intersect(which(data$right), head(seq(nrow(data)), n))
    }
    data <- data[i,,drop = FALSE]
    data$width <- stri_width(data$feature)
    data$color <- color[2 - data$right]
    
    if (length(docname) < 2) {
        docname <- c("Target", "Reference")
    } else if (length(docname) > 2) {
        docname <- c(docname[1], "Reference")
    }

    data$x1 <- ifelse(data$right, abs(data$keyness), abs(data$keyness) * -1)
    data$y1 <- rank(data$keyness, ties.method = "first")
    data$x2 <- rep(limit, nrow(data))
    data$y2 <- data$y
    ggplot() +  
        #geom_point(aes(x1, y1)) + 
        geom_segment(data = data, aes(x = x1, y = y1, xend = x2, yend = y2)) +
        #scale_fill_manual("Document", values = color) +
        xlim(min(data$x1) * 1.1 , max(data$x1) * 1.1) +  
        xlab(measure) +
        geom_label(data = data, aes(x = x1, y = y1, label = feature), label.size = 0, fill = NA,
                   vjust = 'center', hjust = 'outward', color = '#4D4D4D', size = 4) +
        theme_bw() +
        theme(axis.line = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted"))
} 
