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
textplot_keyness.default <- function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    stop(friendly_class_undefined_message(class(x), "textplot_keyness"))
}

#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line geom_bar ylim aes_
#' @export
textplot_keyness.keyness <- function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    
    # extract attribute befor subsetting
    doc <- attr(x, "documents")[1]
    
    # drop infrequent words
    x <- x[(x$n_target + x$n_reference) >= min_count, ]
    
    if (nrow(x) < 1) {
        stop ("Too few words in the documents.")
    }
    
    measure <- colnames(x)[2]
    n <- min(n, nrow(x))
    if (!show_reference) {
        x <- head(x, n)
        x <- data.frame(words = x$feature, val = x[[2]])
        x$words <- factor(x$words, levels = x$words[order(x$val)])
        
        p <- ggplot(data = x, aes(x = x$words, y = x$val)) +
             coord_flip() +
             geom_bar(stat = "identity") +
             ylab(measure) +
             geom_text(aes(label= x$words), hjust = -0.2, vjust = 0.5, size = 3) + 
             theme_bw() +
             theme(axis.line = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(), 
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major.y = element_line(linetype = "dotted"))
    } else {
        if (measure == "pmi") {
            pos_n <- min(n, nrow(x) / 2)
            neg_n <- min(n, nrow(x) / 2)
            feat_top <- head(x, pos_n)
            feat_bottom <- tail(x, neg_n)
            max_y <- max(feat_top[[2]])
            min_y <- min(feat_bottom[[2]])
        } else {
            pos_n <- min(n, sum(x[[2]] >= 0))
            neg_n <- min(n, sum(x[[2]] < 0))
            if ((pos_n == 0) || (neg_n == 0)) 
                stop (" Better plot for one Doc.")
            feat_top <- head(x, pos_n)
            feat_bottom <- tail(x, neg_n)
            max_y <- max(feat_top[[2]])
            min_y <- min(feat_bottom[[2]])
        }
        
        feat_top  <- feat_top[order(feat_top[[2]], decreasing = TRUE), ]
        feat_bottom <- feat_bottom[order(feat_bottom[[2]]), ]
        
        tars <- doc[1]
        if (length(doc) == 2) {
            refs <- doc[2]
        } else {
            refs <- "Reference"
        }
        p1 <- data.frame(x = seq(neg_n + pos_n, 1 + neg_n), y = feat_top[[2]])
        p2 <- data.frame(x = seq(1, neg_n), y = feat_bottom[[2]])
        p <- melt(list(refs = p2, tars = p1), id.vars = "x")
        colnames(p)[4] <- "Document"
        p[p == "refs"] <- refs
        p[p == "tars"] <- tars

        
        ggplot(p, aes_(x = ~x, y = p$value, fill = ~Document)) +  
            geom_bar(stat="identity") + 
            ggplot2::scale_fill_manual("Document", values = c("#003366", "#CC3333")) +
            coord_flip() + 
            # allow extra space for displaying text next to the point
            ylim(min_y * 1.1 , max_y * 1.1) +  
            ylab(measure) +
            geom_text(aes(label = c(feat_bottom$feature, feat_top$feature)), 
                      hjust = ifelse(p$Document == tars, -0.2, 1.2),
                      vjust = 0.5, 
                      colour = ifelse(p$Document == tars, "#CC3333", "#003366"), 
                      size = 3) +
            theme_bw() +
            theme(axis.line = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank(),
                  axis.text.y = ggplot2::element_blank(),
                  axis.ticks.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank(),
                  plot.background = ggplot2::element_blank(),
                  panel.grid.major.y = element_line(linetype = "dotted"))
        
    }
} 
