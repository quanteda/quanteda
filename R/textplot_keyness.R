#' Plot word keyness
#' 
#' Plot the results of a "keyword" of features comparing their differential 
#' associations with a target and a reference group, after calculating keyness
#' using \code{\link{textstat_keyness}}.
#' @param x a return object from \code{\link{textstat_keyness}}
#' @param show_reference logical; if \code{TRUE}, show key reference features in
#'   addition to key target features
#' @param n integer; number of features to plot
#' @param min_count numeric; minimum total count of feature across the target 
#'   and reference categories, for a feature to be included in the plot
#' @return a \pkg{ggplot2} object
#' @export
#' @seealso \code{\link{textstat_keyness}}
#' @keywords textplot
#' @examples
#' \dontrun{
#' # compare Trump v. Obama speeches
#' prescorpus <- corpus_subset(data_corpus_inaugural, 
#'                             President %in% c("Obama", "Trump"))
#' presdfm <- dfm(prescorpus, groups = "President", remove = stopwords("english"),
#'                remove_punct = TRUE)
#' result <- textstat_keyness(presdfm, target = "Trump")
#' 
#' # plot estimated word keyness
#' textplot_keyness(result) 
#' textplot_keyness(result, show_reference = FALSE)
#' }
textplot_keyness <-  function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    UseMethod("textplot_keyness")
}

#' @export
textplot_keyness.default <- function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    stop(friendly_class_undefined_message(class(x), "textplot_keyness"))
}

#' @importFrom stats reorder aggregate
#' @importFrom ggplot2 ggplot aes geom_point element_blank geom_pointrange 
#' @importFrom ggplot2 coord_flip xlab ylab theme_bw geom_text theme geom_point
#' @importFrom ggplot2 facet_grid element_line geom_bar ylim aes_
#' @export
textplot_keyness.keyness <- function(x, show_reference = TRUE, n = 20L, min_count = 2L) {
    
    # extract attribute befor subsetting
    doc <- attr(x, "documents")[1]
    
    # drop infrequent words
    x <- x[(x$n_target + x$n_reference) >= min_count, ]
    x$feature <- factor(x$feature, levels = rev(x$feature))
    
    if (nrow(x) < 1) 
        stop ("Too few words in the documents.")
    
    n <- min(n, nrow(x))
    if (show_reference)
        n <- floor(n / 2)
    
    measure <- colnames(x)[2]
    if (measure == "pmi") {
        limit <- 1.0
    } else {
        limit <- 0
    }
    
    top <- head(x[x[[2]] >= limit,], n)
    bottom <- tail(x[x[[2]] < limit,], n)
    
    if (show_reference) {
        data <- rbind(top, bottom)
    } else {
        data <- top
    }
    
    range_y <- range(data[[2]])
    range_y[1] <- range_y[1] - abs(range_y[1]) * 0.1
    range_y[2] <- range_y[2] + abs(range_y[2]) * 0.1
    
    p <- ggplot(data, aes(x = data[[1]], y = data[[2]])) +
         coord_flip() + 
         ylim(range_y[1], range_y[2]) +  
         ylab(measure) +
         geom_text(aes(label= data[[1]])) + 
         theme_bw() +
         theme(axis.line = ggplot2::element_blank(),
               axis.title.y = ggplot2::element_blank(),
               axis.text.y = ggplot2::element_blank(),
               axis.ticks.y = ggplot2::element_blank(),
               panel.grid.minor.y = ggplot2::element_blank(),
               plot.background = ggplot2::element_blank())
    
    if (show_reference)
        p <- p + geom_hline(yintercept = limit, linetype = "dotted")
    p
} 
