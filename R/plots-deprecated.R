## deprecated plotting functions

#' deprecated plotting functions
#' 
#' Functions with deprecated names for plotting quanteda objects.
#' @param x object to be plotted
#' @param ... additional arguments passed to the new function
#' @name plot-deprecated
#' @keywords internal deprecated plot
NULL

#' @rdname plot-deprecated
#' @details \code{plot.dfm} is the deprecated method to produce a word cloud from 
#' a dfm; use \code{\link{textplot_wordcloud}} instead. 
#' @export
plot.dfm <- function(x, ...) {
    .Deprecated("textplot_wordcloud")
    textplot_wordcloud(x, ...)
}

#' @rdname plot-deprecated
#' @details \code{plot.kwic} is the deprecated method to produce a dispersion
#' plot from a \link{kwic} object; use \code{\link{textplot_xray}} instead. 
#' @export
plot.kwic <- function(x, ...) {
    .Deprecated("textplot_xray")
    textplot_xray(x, ...)
}


#' @rdname plot-deprecated
#' @details \code{plot.textmodel_wordfish_fitted} is the deprecated method to
#'   produce a scale plot with error bars for a fitted wordfish text model object;
#'   \code{\link{textplot_scale1d}} instead.
#' @method plot textmodel_wordfish_fitted
#' @export
plot.textmodel_wordfish_fitted <- function(x, ...) {
    .Deprecated("textplot_scale1d")
    textplot_scale1d(x, ...)
}

