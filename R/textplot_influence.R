#' Influence plot for text scaling models
#' 
#' Plot the results of a fitted scaling model, from (e.g.) a predicted 
#' \link{textmodel_affinity} model.
#' @param x the object output from `influence()` run on the 
#'   fitted or predicted scaling model object to be plotted
#' @param n the number of features whose influence will be plotted
#' @param ... additional arguments passed to \code{\link{plot}}
#' @seealso \code{\link{textmodel_affinity}}
#' @importFrom graphics plot
#' @export
#' @author Patrick Perry and Kenneth Benoit
#' @seealso \code{\link{influence.predict.textmodel_affinity}}
#' @keywords textplot
#' @examples
#' tmod <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
#' pred <- predict(tmod) 
#' textplot_influence(influence(pred))
textplot_influence <- function(x, n = 30, ...) {
    UseMethod("textplot_influence")
}

#' @export
textplot_influence.default <- function(x, n = 30, ...) {
    stop(friendly_class_undefined_message(class(x), "textplot_influence"))
}

#' @export
#' @method textplot_influence influence.predict.textmodel_affinity
textplot_influence.influence.predict.textmodel_affinity <- function(x, n = 30, ...) {
    ans <- summary(x, ...)
    textplot_influence(ans, n, ...)
}

#' @importFrom graphics legend text points
#' @method textplot_influence summary.influence.predict.textmodel_affinity
#' @export
textplot_influence.summary.influence.predict.textmodel_affinity <- function(x, n = 30, ...) {
    word <- x$word[x$support]
    rate <- x$rate[x$support]
    influence <- x$median[x$support]
    direction <- x$direction[x$support]
    imbalance <- influence / rate
    
    x <- log10(rate)
    y <- 100 * influence
    col <- as.integer(direction)
    plot(x, y, type = "n", xlab=expression(Log[10]("Median Rate")),
         ylab=expression("Median Influence" %*% 100))
    
    if (!is.null(n) && !is.na(n)) {
        n <- min(n, nrow(x))
        subset <- rank(-influence, ties.method="first")  <= n
    } else {
        subset <- rep(TRUE, length(word))
    }
    points(x[!subset], y[!subset], cex=0.5, col=col[!subset])
    text(x[subset], y[subset], word[subset], cex=0.75, col=col[subset])
    
    levels <- levels(direction)
    legend("topleft", legend = levels, fill = seq_along(levels), inset=0.05)
}
