#' plot word keyness
#' 
#' Plot the results of textstat_keyness analysis
#' @param x the keyness object to be plotted
#' @param sort if \code{TRUE} (the default), order points from low to high 
#'   score. 
#' @param showBothDoc if \code{TRUE} show dual plot for both documents
#' @param n number of terms to plot.
#' @return a \pkg{ggplot2} object
#' @export
#' @author Kenneth Benoit and Haiyan Wang
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
#' # plot estimated word keyness
#' textplot_keyness(top20) }
#' 
textplot_keyness <-  function(x, sort = TRUE, showBothDoc = FALSE, n = 20) {
    
    UseMethod("textplot_keyness")
}

#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line
#' @export
textplot_keyness <-  function(x, sort = TRUE, showBothDoc = FALSE, n = 20) {
    if (!showBothDoc){
        x <- head(x, n)
        p <- if (sort) {
            ggplot(data = x, aes(x = reorder(rownames(x), x[,1]), y = x[,1]))
        } else {
            ggplot(data = x, aes(x = rownames(x), y = x[,1]))
        }
        p <- p    + coord_flip() +
            geom_point(size=1) +
            ylab(colnames(x)[1]) +
            xlab("Keywords")
        apply_theme(p)
    } else {
        pos_n <- min(n, sum(x[ ,1] >= 0))
        neg_n <- min(n, sum(x[ ,1] < 0))
        topn <- head(x, pos_n)
        tailn <- tail(x, neg_n)
        maxY <- max(max(topn[ ,1]), max(tailn[ ,1]))
        p1 <- if (sort) {
            ggplot(data = topn, aes(x = reorder(rownames(topn), topn[,1]), y = topn[,1]))
        } else {
            ggplot(data = topn, aes(x = rownames(topn), y = topn[,1]))
        }
        p1 <- p1    + coord_flip() + ylim(0, maxY) +
            geom_point(colour="blue", size=1) +
            ylab(colnames(topn)[1]) +
            xlab("Document 1") + 
            theme(axis.title.y = element_text(colour = "blue"))
        apply_theme(p1)
        
        p2 <- if (sort) {
            ggplot(data = tailn, aes(x = reorder(rownames(tailn), abs(tailn[,1])), y = abs(tailn[,1])))
        } else {
            ggplot(data = tailn, aes(x = rownames(tailn), y = tailn[,1]))
        }
        p2 <- p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        p2 <- p2    + coord_flip() + ylim(0, maxY) +
            geom_point(colour="red", size=1) +
            ylab(colnames(tailn)[1])

        ggplot_dual_axis(p1, p2, "y")
    }
} 


##
## common minimal B&W theme
##
apply_theme <- function(p) {
    p + theme_bw() + 
        theme(panel.background = ggplot2::element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(), 
              # panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(), 
              plot.background = element_blank(),
              axis.ticks.y = element_blank(), 
              # panel.spacing = grid::unit(0.1, "lines"),
              panel.grid.major.y = element_line(linetype = "dotted"))
}

## form https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
    
    # Update plot with transparent panel
    plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
    
    grid.newpage()
    
    # Increase right margin if which.axis == "y"
    if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
    
    # Extract gtable
    g1 = ggplot_gtable(ggplot_build(plot1))
    
    g2 = ggplot_gtable(ggplot_build(plot2))
    
    # Overlap the panel of the second plot on that of the first
    pp = c(subset(g1$layout, name == "panel", se = t:r))
    
    g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
    
    # Steal axis from second plot and modify
    axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
    
    ia = which(g2$layout$name == axis.lab)
    
    ga = g2$grobs[[ia]]
    
    ax = ga$children[[2]]
    
    # Switch position of ticks and labels
    if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
    
    ax$grobs = rev(ax$grobs)
    
    if(which.axis == "x") 
        
        ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
            
            ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    
    # Modify existing row to be tall enough for axis
    if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
    
    # Add new row or column for axis label
    if(which.axis == "x") {
        
        g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
        
        g = gtable_add_rows(g, g2$heights[1], 1)
        
        g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
        
    } else {
        
        g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
        
        g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
        
        #g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
        
    }
    
    # Draw it
    grid.draw(g)
    
}
