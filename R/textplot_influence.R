#' Influence plot for text scaling models
#' 
#' Plot the results of a fitted scaling model, from (e.g.) a predicted 
#' \link{textmodel_affinity} model.
#' @param x the fitted or predicted scaling model object to be plotted
#' @param n the number of features whose influence will be plotted
#' @param ... additional arguments passed to \code{\link{plot}}
#' @seealso \code{\link{textmodel_affinity}}
#' @importFrom graphics plot
#' @export
#' @author Patrick Perry and Kenneth Benoit
#' @keywords textplot
textplot_influence <- function(x, n = 30, ...) {
    UseMethod("textplot_influence")
}

#' @export
textplot_influence.default <- function(x, n = 30, ...) {
    stop(friendly_class_undefined_message(class(x), "textplot_influence"))
}

#' @export
textplot_influence.affinity_influence <- function(x, n = 30, ...) {
    ans <- summary(x, ...)
    plot(ans, n, ...)
}

#' @importFrom graphics legend text points
#' @export
textplot_influence.summary_affinity_influence <- function(x, n = 30, ...) {
    word <- x$word[x$support]
    rate <- x$rate[x$support]
    influence <- x$median[x$support]
    direction <- x$direction[x$support]
    imbalance <- influence / rate
    
    x <- log10(rate)
    y <- 100 * influence
    col <- as.integer(direction)
    plot(x, y, type="n", xlab=expression(Log[10]("Median Rate")),
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
