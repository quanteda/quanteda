#' plot word keyness
#' 
#' Plot the results of a keyword analysis using \code{\link{textstat_keyness}}.
#' @param x a return object from \code{\link{textstat_keyness}}
#' @param sort logical; if \code{TRUE} (the default), order points from low to high 
#'   score. 
#' @param show_reference logical; if \code{TRUE}, show key reference features in addition to key target features
#' @param n number of terms to plot
#' @return a \pkg{ggplot2} object
#' @export
#' @author Haiyan Wang
#' @seealso \code{\link{textstat_keyness}}
#' @keywords textplot
#' @examples
#' \dontrun{
#' # compare pre- v. post-war terms using grouping
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' head(mydfm) # make sure 'post-war' is in the first row
#' top20 <- head(result <- textstat_keyness(mydfm), 20)
#' tail10 <- tail(result, 10)
#' 
#' # plot estimated word keyness
#' textplot_keyness(top20) 
#' textplot_keyness(result, show_reference = TRUE)
#' }
textplot_keyness <-  function(x, sort = TRUE, show_reference = FALSE, n = 20) {
    UseMethod("textplot_keyness")
}

#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line
#' @export
textplot_keyness.data.frame <- function(x, sort = TRUE, show_reference = FALSE, n = 20) {

    if (!all(c("p", "n_target", "n_reference") %in% names(x)) | ncol(x) != 4) {
        stop("x must be a return from textstat_keyness")
    }
    
    if (!show_reference) {
        x <- head(x, n)
        
        if (sort) {
            x_reorder <- reorder(rownames(x), x[,1])
        } else {
            x_reorder <- rownames(x)
        }
        p <- ggplot(data = x, aes(x = x_reorder, y = x[,1]))
        
         p    + coord_flip() +
            geom_point(size=1) +
            ylab(colnames(x)[1]) +
            geom_text(aes(label= x_reorder), hjust = ifelse( x[, 1] > x[floor(n/2)+ 1, 1], 1.2, -0.2), 
                      vjust = 0, size = 3) + 
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
            pos_n <- min(n, nrow(x))
            neg_n <- min(n, nrow(x))
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
            # if ((pos_n > 0) & (neg_n > 0)) {
            #     max_Y <- max(max(topn[ ,1]), max(abs(tailn[ ,1])))
            # }
            max_Y <- max(topn[,1])
            min_Y <- min(tailn[,1])
            
            if (sort) {
                #tailn[,1] <- abs(tailn[,1])
                tailn <- tailn[order(tailn[,1]),]
            }
        }
        p1 <- data.frame(x = (neg_n + pos_n) : (1 + neg_n), y = topn[,1])
        p2 <- data.frame(x = neg_n  : 1, y = tailn[,1])
        p <- melt(list(Reference = p2, Target = p1), id.vars = "x")
        colnames(p)[4] <- "Document"
        
        ggplot(p, aes(x, y = value, fill = Document)) +  
            geom_bar(stat="identity") + 
            scale_fill_manual("Document", values = c("Target" = "#CC3333", "Reference" = "#003366")) +
            coord_flip() + 
            ylim(min_Y * 1.1 , max_Y * 1.1) +  ## allow extra space for displaying text next to the point
            ylab(colnames(topn)[1]) +
            geom_text(aes(label= c(rownames(topn), rownames(tailn))), hjust = ifelse( p$Document == "Target", -0.2, 1.2),
                      vjust = 0.5, colour = ifelse(p$Document == "Target", "#CC3333", "#003366"), size = 3) +
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
