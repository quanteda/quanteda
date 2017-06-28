#' plot word keyness
#' 
#' Plot the results of textstat_keyness analysis
#' @param x the keyness object to be plotted
#' @param sort if \code{TRUE} (the default), order points from low to high 
#'   score. 
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
textplot_keyness <-  function(x, sort = TRUE) {
    
    UseMethod("textplot_keyness")
}

#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line
#' @export
textplot_keyness <-  function(x, sort = TRUE) {
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
