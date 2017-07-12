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
        pos_n <- min(n, sum(x[ ,1] >= 0))
        neg_n <- min(n, sum(x[ ,1] < 0))
        if ((pos_n == 0) | (neg_n == 0)) 
            stop (" Better plot for one Doc.")
        topn <- head(x, pos_n)
        tailn <- tail(x, neg_n)
        if ((pos_n > 0) & (neg_n > 0)) {
            max_Y <- max(max(topn[ ,1]), max(abs(tailn[ ,1])))
        }
        
        cols <- c("Target"="#f04546","Reference"="blue")
        
       if (sort) {
            x_reorder <- reorder(rownames(topn), topn[,1])
        } else {
            x_reorder <- rownames(topn)
        }
        p1 <- ggplot(data = topn, aes(x = x_reorder, y = topn[,1]))
        p1 <- p1+    coord_flip() + ylim(0, max_Y) +
            geom_point(aes(colour="Target"), size=1) +
            ylab(colnames(topn)[1]) +
            geom_text(aes(label= x_reorder), hjust = -0.2, #ifelse( topn[, 1] < topn[floor(neg_n/2)+ 1, 1], 1.2, -0.2),
                      vjust = 0, colour = "red", size = 3) +
            scale_colour_manual("", values = cols) +
            theme_bw() +
            theme(axis.line = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank(),
                  axis.text.y = ggplot2::element_blank(),
                  axis.ticks.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank(),
                  plot.background = ggplot2::element_blank(),
                  panel.grid.major.y = element_line(linetype = "dotted"))
        
        ##plot reference
        if (sort) {
            x2_reorder <- reorder(rownames(tailn), abs(tailn[,1]))
        }else{
            x2_reorder <- rownames(tailn)
        }
     
        p2 <- ggplot(data = tailn, aes(x = x2_reorder, y = abs(tailn[,1]))) 
        p2 <- p2+    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            coord_flip() + ylim(0, max_Y) +
            geom_point(aes(colour="Reference"), size = 1) +
            ylab(colnames(tailn)[1]) +
            scale_colour_manual("", values = cols) +
            geom_text(aes(label= x2_reorder), hjust = 1.2, #ifelse( tailn[, 1] > tailn[floor(pos_n/2)+ 1, 1], 1.2, -0.2),
                      vjust = 0, colour = "blue", size = 3) #+ theme_bw() +

        ggplot_dual_axis(p1, p2, "y")
    }
} 

## form https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
#' @importFrom ggplot2 ggplot_gtable ggplot_build ylim element_text element_rect
#' @importFrom grid grid.draw
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @keywords internal
ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
    
    # Update plot with transparent panel
    plot2 <- plot2 + theme(panel.background = element_rect(fill = NA))
    
    grid::grid.newpage()
    
    # Increase right margin if which.axis == "y"
    if (which.axis == "y") plot1 <- plot1 + theme(plot.margin = grid::unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
    
    # Extract gtable
    g1 <- ggplot_gtable(ggplot_build(plot1))
    g2 <- ggplot_gtable(ggplot_build(plot2))
    
    # Overlap the panel of the second plot on that of the first
    pp <- c(subset(g1$layout, g1$layout$name == "panel"))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
    
    # Steal axis from second plot and modify
    axis.lab <- ifelse(which.axis == "x", "axis-b", "axis-l")
    ia <- which(g2$layout$name == axis.lab)
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    
    # Switch position of ticks and labels
    if (which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
    ax$grobs = rev(ax$grobs)
    if (which.axis == "x") {
        ax$grobs[[2]]$y = ax$grobs[[2]]$y - grid::unit(1, "npc") + grid::unit(0.15, "cm") 
    } else {
        ax$grobs[[1]]$x = ax$grobs[[1]]$x - grid::unit(1, "npc") + grid::unit(0.15, "cm")
    }
    
    # Modify existing row to be tall enough for axis
    if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
    
    # Add new row or column for axis label
    # if (which.axis == "x") {
    #     g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    #     g = gtable_add_rows(g, g2$heights[1], 1)
    #     g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    # } else {
    #     g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    #     g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    #     #g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    # }
    # 
    # Draw it
    grid.draw(g)
}
