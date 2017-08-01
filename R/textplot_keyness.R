#' plot word keyness
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

#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line geom_bar ylim aes_
#' @export
textplot_keyness.data.frame <- function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    
    if (!all(c("p", "n_target", "n_reference") %in% names(x)) | ncol(x) != 4) {
        stop("x must be a return from textstat_keyness")
    }
    
    # filter out the rare words
    x <- x[(x$n_target + x$n_reference) >= min_count, ]
    
    if (nrow(x) < 1) {
        stop ("Too few words in the documents.")
    }
    
    n <- min(n, nrow(x))
    if (!show_reference) {
        x <- head(x, n)
        measure <- colnames(x)[1]
        x  <- data.frame(words = rownames(x), val = x[, 1])
        x$words <- factor(x$words, levels = x$words[order(x$val)])
        
        p <- ggplot(data = x, aes(x = x$words, y = x$val))
        
        p   + coord_flip() +
            geom_bar(stat="identity") +
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
        if (colnames(x)[1] == "pmi"){
            pos_n <- min(n, nrow(x)/2)
            neg_n <- min(n, nrow(x)/2)
            topn <- head(x, pos_n)
            tailn <- tail(x, neg_n)
            max_Y <- max(topn[, 1])
            min_Y <- min(tailn[, 1])
        } else {
            pos_n <- min(n, sum(x[ ,1] >= 0))
            neg_n <- min(n, sum(x[ ,1] < 0))
            if ((pos_n == 0) | (neg_n == 0)) 
                stop (" Better plot for one Doc.")
            topn <- head(x, pos_n)
            tailn <- tail(x, neg_n)
            max_Y <- max(topn[, 1])
            min_Y <- min(tailn[, 1])
        }   
        topn  <- topn[order(-topn[, 1]), ]
        tailn <- tailn[order(tailn[, 1]), ]
        
        Tars <- attr(x, "documents")[1]
        if (length(attr(x, "documents")) == 2){
            Refs <- attr(x, "documents")[2]
        } else {
            Refs <- "Reference"
        }
        p1 <- data.frame(x = (neg_n + pos_n) : (1 + neg_n), y = topn[,1])
        p2 <- data.frame(x = 1 : neg_n, y = tailn[,1])
        p <- melt(list(Refs = p2, Tars = p1), id.vars = "x")
        colnames(p)[4] <- "Document"
        p[p=="Refs"] <- Refs
        p[p=="Tars"] <- Tars

        
        ggplot(p, aes_(x = ~x, y = p$value, fill = ~Document)) +  
            geom_bar(stat="identity") + 
            ggplot2::scale_fill_manual("Document", values = c("#003366", "#CC3333")) +
            coord_flip() + 
            ylim(min_Y * 1.1 , max_Y * 1.1) +  ## allow extra space for displaying text next to the point
            ylab(colnames(topn)[1]) +
            geom_text(aes(label= c(rownames(tailn), rownames(topn))), hjust = ifelse( p$Document == Tars, -0.2, 1.2),
                      vjust = 0.5, colour = ifelse(p$Document == Tars, "#CC3333", "#003366"), size = 3) +
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
