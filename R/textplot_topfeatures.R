#' plot the most frequent features in a dfm
#' 
#' Plots the most frequent features in a dfm in descending order. Given that this returns a ggplot object, you can modify the plot by adding ggplot layers (see example).
#' @param x a dfm object
#' @param n how many top features should be returned
#' @param sort whether to sort the documents on the x-axis according to the frequency
#' @param angle The angle of the document labels on the x-axis
#' @return a **ggplot2** object
#' @author Stefan Müller
#' @seealso \code{\link{dfm}}, \code{\link{dfm_weight}}
#' @examples 
#' \dontrun{
#' dfm_inaugural <- dfm(data_corpus_inaugural, remove = stopwords("english"), 
#'                     remove_punct = TRUE)
#' textplot_topfeatures(x = dfm_inaugural, n = 50, angle = 45)
#' }
#' @export
#' @keywords plot

#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 element_line scale_x_discrete
#' @export

textplot_topfeatures <- function(x, n, sort = TRUE, angle = 90) {
  
  x_topfeatures <- topfeatures(x, n)
  
  data <- data.frame(
    list(
      term = names(x_topfeatures),
      frequency = unname(x_topfeatures)
    )
  )
  
  if (sort == TRUE) {
    data$term <- with(data, reorder(term, -frequency))
  }
  
  p <- ggplot(data = data) + 
    geom_point(aes(x = term, y = frequency)) +
    xlab("Term") +
    ylab("Frequency")
  
  apply_theme(p) + theme(axis.text.x = element_text(angle=angle, hjust=1))

}

##
## common minimal B&W theme
##
apply_theme <- function(p) {
  p + theme_bw() + 
    theme(panel.background = ggplot2::element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          plot.background = element_blank(),
          axis.ticks.y = element_blank(), 
          panel.grid.major.y = element_line(linetype = "dotted"))
}
