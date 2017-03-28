#' plot a fitted scaling model
#' 
#' Plot the results of a fitted scaling model, from (e.g.) a predicted 
#' \link{textmodel_wordscores} model or a fitted \link{textmodel_wordfish}
#' model. Either document or feature parameters may be plotted: an ideal
#' point-style plot (estimated document position plus confidence interval on the
#' x-axis, document labels on the y-axis) with optional renaming and sorting, or
#' as a plot of estimated feature-level parameters (estimated feature positions
#' on the x-axis, and a measure of relative frequency or influence on the
#' y-axis, with feature names replacing plotting points with some being chosen
#' by the user to be highlighted).
#' @param x the fitted or predicted scaling model object to be plotted
#' @param margin \code{"documents"} to plot estimated document scores (the
#'   default) or \code{"features"} to plot estimated feature scores by a measure
#'   of relative frequency
#' @param sort if \code{TRUE} (the default), order points from low to high 
#'   score. If a vector, order according to these values from low to high. Only 
#'   applies when \code{margin = "documents"}.
#' @param groups optional grouping variable for sorting categories of the documents.  
#' Only applies when \code{margin = "documents"}.
#' @param doclabels a vector of names for document; if left NULL (the default), 
#'   docnames will be used
#' @param highlighted a vector of feature names to draw attention to in a 
#'   feature plot; only applies if \code{margin = "features"}
#' @param highlighted_color color for highlighted terms in \code{highlighted}
#' @param alpha A number between 0 and 1 (default 0.5) representing the level of
#'   alpha transparency used to overplot feature names in a feature plot; only 
#'   applies if \code{margin = "features"}
#' @return a \pkg{ggplot2} object
#' @export
#' @author Kenneth Benoit, Stefan Müller, and Adam Obeng
#' @keywords textplot
#' @examples
#' ie_dfm <- dfm(data_corpus_irishbudget2010)
#' doclab <- apply(docvars(data_corpus_irishbudget2010, c("name", "party")), 
#'                 1, paste, collapse = " ")
#' 
#' ## wordscores
#' refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
#' ws <- textmodel(ie_dfm, refscores, model="wordscores", smooth = 1)
#' pred <- predict(ws)
#' # plot estimated word positions
#' textplot_scale1d(pred, margin = "features", 
#'                  highlighted = c("minister", "have", "our", "budget"))
#' # plot estimated document positions
#' textplot_scale1d(pred, margin = "documents",
#'                  doclabels = doclab,
#'                  groups = docvars(data_corpus_irishbudget2010, "party"))
#'
#' ## wordfish
#' wfm <- textmodel_wordfish(dfm(data_corpus_irishbudget2010), dir = c(6,5))
#' # plot estimated document positions
#' textplot_scale1d(wfm, doclabels = doclab)
#' textplot_scale1d(wfm, doclabels = doclab,
#'                  groups = docvars(data_corpus_irishbudget2010, "party"))
#' # plot estimated word positions
#' textplot_scale1d(wfm, margin = "features", 
#'                  highlighted = c("government", "global", "children", 
#'                                  "bank", "economy", "the", "citizenship",
#'                                  "productivity", "deficit"))
textplot_scale1d <- function(x, margin = c("documents", "features"), doclabels = NULL, 
                             sort = TRUE, groups = NULL, 
                             highlighted = NULL, alpha = 0.7, 
                             highlighted_color = "black") {
    UseMethod("textplot_scale1d")
}

#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line
#' @export
textplot_scale1d.textmodel_wordfish_fitted <-  function(x, 
                                                        margin = c("documents", "features"), 
                                                        doclabels = NULL, 
                                                        sort = TRUE, groups = NULL, 
                                                        highlighted = NULL, alpha = 0.7, 
                                                        highlighted_color = "black") {
    margin <- match.arg(margin)
    if (is.null(doclabels)) doclabels <- x@docs
    
    if (margin == "documents") {
        p <- textplot_scale1d_documents(coef(x)$coef_document, coef(x)$coef_document_se, 
                                        doclabels = doclabels, sort = sort, groups = groups) +
            ylab("Estimated theta")
    } else if (margin == "features") {
        p <- textplot_scale1d_features(coef(x)$coef_feature, 
                                       weight = coef(x)$coef_feature_offset, 
                                       featlabels = x@features,
                                       highlighted = highlighted, alpha = alpha,
                                       highlighted_color = highlighted_color) +
            xlab("Estimated beta") +
            ylab("Estimated psi")
    } 
    apply_theme(p)
} 

     
#' @noRd
#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line
#' @export
textplot_scale1d.textmodel_wordscores_predicted <- function(x, 
                                                            margin = c("documents", "features"), 
                                                            doclabels = NULL, 
                                                            sort = TRUE, groups = NULL, 
                                                            highlighted = NULL, alpha = 0.7, 
                                                            highlighted_color = "black") {
    margin <- match.arg(margin)
    if (is.null(doclabels)) doclabels <- docnames(x@newdata)
    
    if (margin == "documents") {
        p <- textplot_scale1d_documents(coef(x)$coef_document, coef(x)$coef_document_se, 
                                        doclabels = doclabels, sort = sort, groups = groups) +
            
              ylab("Document position")
        
    } else if (margin == "features") {
        p <- textplot_scale1d_features(x@Sw, 
                                       weight = log(colSums(x@x[, names(x@Sw)])),
                                       featlabels = featnames(x@x[, names(x@Sw)]),
                                       highlighted = highlighted, alpha = alpha,
                                       highlighted_color = highlighted_color) +
            xlab("Word score") +
            ylab("log(term frequency)") 
    } 
    apply_theme(p)
}


##
## internal function to plot document scaling
##
textplot_scale1d_documents <- function(x, se, doclabels, sort = TRUE, groups = NULL) {

    if (!is.null(doclabels))
        stopifnot(length(doclabels) == length(x))
    
    if (sort & !is.null(groups)) {
        temp_medians <- aggregate(x, list(groups), median, na.rm = TRUE)
        groups <- factor(groups, levels = temp_medians[order(temp_medians$x, decreasing = TRUE), 1])
    }
    
    theta <- lower <- upper <- NULL
    results <- data.frame(doclabels = doclabels, 
                          theta = x, 
                          lower = x - 1.96 * se, 
                          upper = x + 1.96 * se)
    if (!is.null(groups))
        results$groups <- groups
    
    p <- if (sort) {
        ggplot(data = results, aes(x = reorder(doclabels, theta), y = theta))
    } else {
        ggplot(data = results, aes(x = doclabels, y = theta))
    }
    
    p <- p + 
        coord_flip() + 
        { if (!is.null(groups))
            facet_grid(as.factor(groups) ~ ., scales = "free_y", space = "free") } +       
        geom_pointrange(aes(ymin = lower, ymax = upper), lwd = .25, fatten = .4) + 
        geom_point(size = 1) +
        xlab(NULL)
    p
}

##
## internal function to plot document scaling
##
textplot_scale1d_features <- function(x, weight, featlabels,
                                      highlighted = NULL, alpha = 0.7, 
                                      highlighted_color = "black") {
        
    beta <- psi <- feature <- NULL
    results <- data.frame(feature = featlabels, 
                          psi = weight,
                          beta = x)
    p <- ggplot(data = results, aes(x = beta, y = psi, label = feature)) + 
        geom_text(colour = "grey70") +
        geom_text(aes(beta, psi, label = feature), 
                  data = results[results$feature %in% highlighted,],
                  color = highlighted_color) +
        xlab("Beta") +
        ylab("Psi") + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p
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
