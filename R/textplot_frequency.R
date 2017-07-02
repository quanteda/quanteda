#' plot the frequency of a term across documents
#' 
#' Plots the absolute or relative frequency of a term in all documents of a document-feature matrix.
#' Given that this returns a ggplot object, you can modify the plot by adding ggplot layers (see example).
#' @param x a dfm object
#' @param term the term to be plotted
#' @param type the frequency type to be plotted. Options are frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf". See \code{\link{dfm_weight}} for more information
#' @param doclabels a vector of names for document; if left NULL (the default), docnames will be used
#' @param sort whether to sort the documents on the x-axis according to the frequency of the term
#' @param angle The angle of the document labels on the x-axis
#' @return a **ggplot2** object
#' @author Stefan Müller
#' @seealso \code{\link{dfm}}, \code{\link{dfm_weight}}
#' @examples 
#' \dontrun{
#' doclab <- apply(docvars(data_corpus_irishbudget2010, c("name", "party")), 
#'                1, paste, collapse = " ")
#'
#' textplot_frequency(x = dfm(data_corpus_irishbudget2010),
#'                   term = "tax", sort = TRUE, doclabels = doclab)
#'
#' textplot_frequency(x = dfm(data_corpus_irishbudget2010), type = "relMaxFreq",
#'                   term = "tax", sort = FALSE, doclabels = doclab)
#'
#' data_corpus_inauguralPost80 <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' 
#' textplot_frequency(dfm(data_corpus_inauguralPost80), "america", sort = TRUE) +
#'  theme_minimal()
#' }
#' @export
#' @keywords plot

#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_point element_blank 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw theme geom_point
#' @importFrom ggplot2 scale_x_discrete
#' @export


textplot_frequency <- function (x, term, sort = TRUE, doclabels = NULL, type = 'frequency', angle = 90) {

  if (type != 'frequency') {
    x <- dfm_weight(x, type = as.character(type))
  }
  
  if (is.null(doclabels)) doclabels <- x@Dimnames$docs
  
  feature_data_frame <- data.frame(list(
    doclabels = doclabels,
    frequency = unname(as.matrix(x[, as.character(term)]))))

  p <- if (sort) {
    ggplot(data = feature_data_frame, aes(x = reorder(doclabels, -frequency), y = frequency))
  } else {
    ggplot(data = feature_data_frame, aes(x = doclabels, y = frequency))
  }
  
  p <-  p + geom_point() +
    xlab("Document") +
    ylab(NULL) +
    if (type == "frequency") {
      ggtitle(paste0("Frequency", " (“", term, "”)", sep = ""))
    } else if (type == "relFreq") {
      ggtitle(paste0("Relative frequency", " (“", term, "”)", sep = ""))
    } else if (type == "relMaxFreq") {
      ggtitle(paste0("Relative maximum frequency", " (“", term, "”)", sep = ""))
    } else if (type == "logFreq") {
      ggtitle(paste0("Frequency (log)", " (“", term, "”)", sep = ""))
    } else
      ggtitle(paste0("Term-frequency * inverse document frequency", " (“", term, "”)", sep = ""))

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
