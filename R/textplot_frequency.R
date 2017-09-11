#' plot the frequency of a feature across documents
#' 
#' Plots the absolute or relative frequency of a feature in all documents of a 
#' document-feature matrix. Given that this returns a ggplot object, you can 
#' modify the plot by adding ggplot layers (see example).
#' @param x a dfm or weighted dfm object
#' @param margin "documents" to plot the most frequent features across documents or groups
#'   or "features" to plot the occurrence of one or more features
#' @param features the feature or features whose frequency will be plotted
#' @param group a vector of names to group the dfm; if left \code{NULL} (the
#'   default) the feature frequencies will be estimated across all documents
#' @param n how many top features should be returned (only relevant for margin = "documents")
#'
#' @return a \pkg{ggplot2} object
#' @author Stefan Müller and Kenneth Benoit
#' @seealso \code{\link{textstat_frequency}}, \code{\link{dfm}}, \code{\link{dfm_weight}}
#' @examples 
#' \dontrun{
#' # transform to dfm and get grouping doclabels
#' ie_dfm <- dfm(data_corpus_irishbudget2010[1:6], 
#'               remove = stopwords("english"),
#'               remove_punct = TRUE)
#' ie_groups <- apply(docvars(data_corpus_irishbudget2010, c("name", "party")), 
#'                 1, paste, collapse = " ")[1:6]
#' # plot 20 most frequent features aggregated for entire dfm
#' textplot_frequency(ie_dfm, margin = "documents", n = 20)
#' 
#' # plot 10 most frequent features for each group
#' textplot_frequency(ie_dfm, margin = "documents",
#'                    groups = ie_groups, n = 10)
#' 
#' # plot occurrence of selected features aggregated for entire dfm
#' textplot_frequency(ie_dfm, margin = "features", 
#'                    features = c("tax", "ireland", "increase"))
#' 
#' # plot occurrence of selected features for each group
#' textplot_frequency(ie_dfm, margin = "features", 
#'                        features = c("tax", "ireland", "increase"), 
#'                        groups = ie_groups)
#' 
#' # plot frequencies based on weighted dfm
#' ie_dfm_relfreq <- dfm_weight(ie_dfm, type = "relfreq")
#' 
#' textplot_frequency(ie_dfm_relfreq, n = 10, margin = "documents",
#'                        groups = ie_groups) +
#'   ylab("Relative Frequency")
#' 
#' 
#' @export
textplot_frequency <- function(x, margin = c("documents", "features"),
                               type = NULL,
                               n = 15, groups = NULL,
                               features = NULL) {
  UseMethod("textplot_frequency")
}

#' @noRd
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_wrap element_line
#' @export

textplot_frequency.dfm <- function(x, margin = c("documents", "features"),
                                   type = NULL,
                                   n = 15, groups = NULL,
                                   features = NULL) {
  
  if (margin == "documents") {
    
    data_freq <- textstat_frequency(x, n = n, groups = groups)
    
    data_freq <- data_freq[seq(dim(data_freq)[1],1),] # reorder rows for plotting
    
    data_freq$order <- 1:nrow(data_freq)
    
    if (is.null(groups)) {
      p <- ggplot(data_freq, aes(x = reorder(feature, -rank), y = frequency))
    }
    
    else {
      p <- ggplot(data_freq, aes(x = order, y = frequency)) +
        facet_wrap(~ group, scales = "free") +
        scale_x_continuous(breaks = data_freq$order, labels = data_freq$feature)
    }
    
    p <- p +
      geom_point() +
      coord_flip() +
      xlab(NULL) +
      ylab("Frequency")
    
    apply_theme(p)
    
  } else if (margin == "features") {
    
    data_freq <- textstat_frequency(x, n = length(x), groups = groups)
    
    data_freq <- data_freq[seq(dim(data_freq)[1],1),] # reorder rows for plotting
    
    data_subset <- subset(data_freq, feature %in% as.list(features))
    
    if (length(features) == 1) {
      data_subset <- reorder(data_subset, frequency)
    }
    
    if (!is.null(groups) & length(features) > 1) {
      p <- ggplot(data_subset, aes(x = group, y = frequency)) + 
        facet_wrap(~ feature)
    }
    
    if (length(features) == 1 & !is.null(groups)) {
      p <- ggplot(data_subset, aes(x = group, y = frequency))
    }
    
    if (length(features) > 1 & is.null(groups)) {
      p <- ggplot(data_subset, aes(x = feature, y = frequency))
    }
    
    else {
      p <- ggplot(data_subset, aes(x = group, y = frequency))
    }
    
    p <- p +
      geom_point() +
      coord_flip() +
      xlab(NULL) +
      ylab("Frequency")
    
    apply_theme(p)
    
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
