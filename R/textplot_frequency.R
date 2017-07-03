#' plot the frequency of a feature across documents
#' 
#' Plots the absolute or relative frequency of a feature in all documents of a 
#' document-feature matrix. Given that this returns a ggplot object, you can 
#' modify the plot by adding ggplot layers (see example).
#' @param x a dfm object
#' @param feature the feature whose frequency will be plotted
#' @param type the frequency type to be plotted. Options are "frequency", 
#'   "relFreq", "relMaxFreq", "logFreq", "tfidf". See \code{\link{dfm_weight}} 
#'   for legal values.
#' @param doclabels a vector of names for document; if left \code{NULL} (the
#'   default), \code{docnames(x)} will be used
#' @param sort logical; if \code{TRUE}, sort the documents on the x-axis
#'   according to the frequency of the feature
#' @return a \pkg{ggplot2} object
#' @author Stefan Müller
#' @seealso \code{\link{dfm}}, \code{\link{dfm_weight}}
#' @examples 
#' \dontrun{
#' doclab <- apply(docvars(data_corpus_irishbudget2010, c("name", "party")), 
#'                 1, paste, collapse = " ")
#' textplot_frequency(x = dfm(data_corpus_irishbudget2010),
#'                    feature = "tax", sort = TRUE, doclabels = doclab)
#' textplot_frequency(x = dfm(data_corpus_irishbudget2010),
#'                    feature = "tax", sort = FALSE, doclabels = doclab)
#' textplot_frequency(x = dfm(data_corpus_irishbudget2010), type = "logFreq",
#'                    feature = "tax", doclabels = doclab)
#'                   
#' data_corpus_inauguralPost80 <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' textplot_frequency(dfm(data_corpus_inauguralPost80), "america", sort = TRUE)
#' }
#' @export
#' @keywords plot

#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_point element_blank 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw theme geom_point
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 element_text ggtitle
#' @export
textplot_frequency <- function(x, feature, type = "frequency", sort = TRUE, doclabels = docnames(x)) {
  UseMethod("textplot_frequency")
}

#' @noRd
#' @export
textplot_frequency.dfm <- function(x, feature, type = "frequency", sort = TRUE, doclabels = docnames(x)) {
  
  type <- match.arg(type, c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf"))
  if (type != "frequency") {
    x <- dfm_weight(x, type = as.character(type))
  }
  
  if(!(as.character(feature) %in% colnames(x))) {
    stop("feature is not part of the dfm")
  }
  
  if (length(doclabels) != ndoc(x)) {
    stop("custom doclabels do not have same length as documents in dfm")
  }
  
  freq <- NULL
  feature_data_frame <- data.frame(
    doclabels = doclabels,
    freq =  as.vector(x[, feature])
  )
  
  p <- if (sort == TRUE) {
    ggplot(data = feature_data_frame, aes(x = reorder(doclabels, freq), y = freq))
  } else {
    ggplot(data = feature_data_frame, aes(x = doclabels, y = freq))
  }
  
  p <-  p + geom_point() +
    coord_flip() + 
    xlab(NULL) +
    if (type == "frequency") {
      ylab("Frequency")
    } else if (type == "relFreq") {
      ylab("Relative frequency")
    } else if (type == "relMaxFreq") {
      ylab("Relative maximum frequency")
    } else if (type == "logFreq") {
      ylab("Frequency (log)")
    } else
      ylab("Term-frequency * inverse document frequency")
  
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
          panel.grid.minor.y = element_blank(), 
          plot.background = element_blank(),
          axis.ticks.y = element_blank(), 
          panel.grid.major.y = element_line(linetype = "dotted"))
}
