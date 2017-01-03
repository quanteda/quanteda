#' plot features as a wordcloud
#' 
#' Plot a \link{dfm} or \link{tokens} object as a wordcloud, where the feature 
#' labels are plotted with their sizes proportional to their numerical values in
#' the dfm.  When \code{comparison = TRUE}, it plots comparison word clouds by 
#' document.
#' @details The default is to plot the word cloud of all features, summed across
#'   documents.  To produce word cloud plots for specific document or set of 
#'   documents, you need to slice out the document(s) from the dfm or tokens
#'   object.
#'   
#'   Comparison word cloud plots may be plotted by setting \code{comparison = 
#'   TRUE}, which plots a separate grouping for \emph{each document} in the dfm.
#'   This means that you will need to slice out just a few documents from the 
#'   dfm, or to create a dfm where the "documents" represent a subset or a 
#'   grouping of documents by some document variable.
#' @param x a dfm object
#' @param comparison if \code{TRUE}, plot a 
#'   \code{\link[wordcloud]{comparison.cloud}} instead of a simple wordcloud, 
#'   one grouping per document
#' @param ... additional parameters passed to to \link[wordcloud]{wordcloud} or 
#'   to \link{text} (and \link{strheight}, \link{strwidth})
#' @seealso \code{\link[wordcloud]{wordcloud}}, 
#'   \code{\link[wordcloud]{comparison.cloud}}
#' @examples
#' # plot the features (without stopwords) from Obama's two inaugural addresses
#' mydfm <- dfm(corpus_subset(data_corpus_inaugural, President=="Obama"), verbose = FALSE,
#'              remove = stopwords("english"))
#' textplot_wordcloud(mydfm)
#' 
#' # plot in colors with some additional options passed to wordcloud
#' textplot_wordcloud(mydfm, random.color = TRUE, rot.per = .25, 
#'                    colors = sample(colors()[2:128], 5))
#' 
#' \dontrun{
#' # comparison plot of Irish government vs opposition
#' docvars(data_corpus_irishbudget2010, "govtopp") <- 
#'     factor(ifelse(data_corpus_irishbudget2010[, "party"] %in% c("FF", "Green"), "Govt", "Opp"))
#' govtoppDfm <- dfm(data_corpus_irishbudget2010, groups = "govtopp", verbose = FALSE)
#' textplot_wordcloud(tfidf(govtoppDfm), comparison = TRUE)
#' # compare to non-tf-idf version
#' textplot_wordcloud(govtoppDfm, comparison = TRUE)
#' }
#' @export
#' @keywords plot
textplot_wordcloud <- function(x, comparison = FALSE, ...) {
    
    if (!is.dfm(x) & !is.tokens(x))
        stop("x must be a dfm or tokens object")
    
    if (is.tokens(x))
        x <- dfm(x, verbose = FALSE)
        
    if (comparison) {
        if (ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud::comparison.cloud(t(as.matrix(x)), ...)
    } else {
        wordcloud::wordcloud(featnames(x), colSums(x), ...)
    }
}




#' plot the dispersion of key word(s)
#' 
#' Plots a dispersion or "x-ray" plot of selected word pattern(s) across one or
#' more texts. The format of the plot depends on the number of \link{kwic} class
#' objects passed: if there is only one document, keywords are plotted one below
#' the other. If there are multiple documents the documents are plotted one
#' below the other, with keywords shown side-by-side. Given that this returns a
#' ggplot object, you can modify the plot by adding ggplot layers (see example).
#' @param ... any number of \link{kwic} class objects
#' @param scale whether to scale the token index axis by absolute position of the token in the 
#' document or by relative position. Defaults are absolute for single document and relative for
#' multiple documents.
#' @param sort whether to sort the rows of a multiple document plot by document name
#' @author Adam Obeng
#' @return \code{plot.kwic} returns a ggplot object
#' @examples 
#' \dontrun{
#' data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' # compare multiple documents
#' textplot_xray(kwic(data_corpus_inauguralPost70, "american"))
#' textplot_xray(kwic(data_corpus_inauguralPost70, "american"), scale = "absolute")
#' # compare multiple terms across multiple documents
#' textplot_xray(kwic(data_corpus_inauguralPost70, "america*"), 
#'               kwic(data_corpus_inauguralPost70, "people"))
#' 
#' # how to modify the ggplot with different options
#' library(ggplot2)
#' g <- textplot_xray(kwic(data_corpus_inauguralPost70, "american"), 
#'                    kwic(data_corpus_inauguralPost70, "people"))
#' g + aes(color = keyword) + scale_color_manual(values = c('red', 'blue'))
#' }
#' @export
#' @keywords plot
textplot_xray <- function(..., scale = c("absolute", "relative"), sort = FALSE) {
    UseMethod("textplot_xray")
}
    
#' @rdname textplot_xray
#' @noRd
#' @export
textplot_xray.kwic <- function(..., scale = c("absolute", "relative"), sort = FALSE) {
    
    if (!requireNamespace("ggplot2", quietly = TRUE))
        stop("You must have ggplot2 installed to make a dispersion plot.")
    if(!requireNamespace("grid", quietly = TRUE)) 
        stop("You must have grid installed to make a dispersion plot.")
    
    position <- keyword <- docname <- ntokens <- NULL    
    
    kwics <- list(...)
    if (!all(sapply(kwics, is.kwic)))
        stop("objects to plot must be kwic objects")

    # create a data.table from the kwic arguments
    x <- data.table(do.call(rbind, kwics))
    # get the vector of ntokens
    ntokensByDoc <- unlist(lapply(kwics, attr, "ntoken"))
    # add ntokens to data.table as an indexed "merge"
    x[, ntokens := ntokensByDoc[as.character(x[, docname])]]
    
    # replace "found" keyword with patterned keyword
    x[, keyword := unlist(sapply(kwics, function(l) rep(attr(l, "keyword"), nrow(l))))]

    # pre-emptively convert keyword to factor before ggplot does it, so that we
    # can keep the order of the factor the same as the order of the kwic objects
    x[, keyword := factor(keyword, levels = unique(keyword))]

    multiple_documents <- length(unique(x$docname)) > 1

    # Deal with the scale argument:
    # if there is a user-supplied value, use that after passing through match.argj
    # if not, use relative for multiple documents and absolute for single documents
    if (!missing(scale)) {
        scale <- match.arg(scale)
    }
    else {
        if (multiple_documents) {
            scale <- "relative"
        }
        else {
            scale <- "absolute"
        }
    }

    if (sort) 
        x[, docname:=factor(docname, levels=levels(docname)[order(levels(docname))])]

    # convert character positions to first integer value in range
    if (is.character(x[, position]))
        x[, position := as.integer(sapply(strsplit(position, ":"), "[", 1))]

    if (scale == 'relative')
        x[, position := position/ntokens]

    plot <- ggplot2::ggplot(x, ggplot2::aes(x=position, y=1)) + ggplot2::geom_segment(ggplot2::aes(xend=position, yend=0)) + 
        ggplot2::theme(axis.line = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(), 
                       plot.background = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(), 
                       axis.text.y = ggplot2::element_blank(),
                       panel.spacing = grid::unit(0.1, "lines"), 
                       panel.border = ggplot2::element_rect(colour = "gray", fill=NA),
                       strip.text.y = ggplot2::element_text(angle=0)
        ) 
    
    if (scale == 'absolute')
        plot <- plot + ggplot2::geom_rect(ggplot2::aes(xmin=ntokens, xmax=max(x$ntokens), 
                                                       ymin=0, ymax=1), fill = 'gray90')

    if (multiple_documents) {
        # If there is more than one document, put documents on the panel y-axis and keyword(s)
        # on the panel x-axis
        plot <- plot + ggplot2::facet_grid(docname ~ keyword) + 
            ggplot2::labs(y = 'Document', title = paste('Lexical dispersion plot'))
    }
    else {
        # If not, put keywords on the panel y-axis and the document name in the title
        plot <- plot + ggplot2::facet_grid(keyword~.) + 
            ggplot2::labs(y = '', title = paste('Lexical dispersion plot, document:', x$docname[[1]]))
    }
    
    if (scale == 'relative') {
        plot <- plot + ggplot2::labs(x='Relative token index')
    }
    else {
        plot <- plot + ggplot2::labs(x='Token index')
    }

    plot
}


#' plot a fitted wordfish model
#' 
#' Plot a fitted wordfish model, either as an ideal point-style plot (theta plus
#' confidence interval on the x-axis, document labels on the y) with optional 
#' renaming and sorting, or as a plot of estimated feature-level parameters
#' (beta on the x, psi on the y, feature names over-plotted with alpha
#' transparency, optionally some highlighted) as in Slapin and Proksch, 2008.
#' @param x the fitted \link{textmodel_wordfish} object to be plotted
#' @param margin \code{"documents"} to plot document scores theta (the default)
#'   or \code{"features"} to plot psi against beta parameters
#' @param sort if \code{TRUE} (the default), order points from low to high score. If a 
#'   vector, order according to these values from low to high. Only applies when
#'   \code{margin = "documents"}
#' @param doclabels a vector of names for document. If left NULL (the default), 
#'   ordinary document names will be used.
#' @param mar_left an overridden left margin, passed to \code{par} (default 
#'   8.1). This overrides R's default 4.1, which is typically too cramped for 
#'   document names.
#' @param highlighted a vector of feature names to draw attention to in a feature 
#'   plot. Only applies if \code{margin = "features"}.
#' @param alpha A number between 0 and 1 (default 0.5) representing the level of
#'   alpha transparency used to overplot feature names in a feature plot. Only 
#'   applies if \code{margin = "features"}.
#' @param ... additional arguments passed to \code{\link{plot}}
#' @export
#' @references Jonathan Slapin and Sven-Oliver Proksch.  2008. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{American 
#'   Journal of Political Science} 52(3):705-772.
#' @author Adam Obeng
#' @importFrom graphics plot segments axis points par text
#' @importFrom grDevices rgb
#' @examples 
#' postwar <- dfm_trim(dfm(data_corpus_inaugural[41:57]), min_count = 5, min_docfreq = 2)
#' mod <- textmodel(postwar, model = "wordfish")
#' textplot_scale1d(mod, sort = FALSE)
#' textplot_scale1d(mod, sort = TRUE)
#' textplot_scale1d(mod, margin = "features", 
#'                  highlighted = c("government", "countries", "children", 
#'                              "the", "nuclear", "federal"))
textplot_scale1d <- function(x, margin = c("documents", "features"), doclabels = NULL,
                                         sort = TRUE, mar_left = 8, highlighted = NULL, alpha = 0.5, ...) {
    if (!is(x, "textmodel_wordfish_fitted"))
        stop("x must be a fitted textmodel_wordfish object")
    
    margin <- match.arg(margin)
    if (margin == "documents") {
        n <- length(x@theta)
        
        if (is.null(doclabels)) 
            doclabels <- docnames(x@x)
        stopifnot(length(doclabels)==n)
        
        results <- data.frame(doclabels = doclabels,
                              theta = x@theta,
                              lower = x@theta - 1.96*x@se.theta,
                              upper = x@theta + 1.96*x@se.theta)
        sorting <- 1:n
        if (length(sort) > 1){
            stopifnot(length(sort) == n)
            sorting <- order(sort)
        } else if (sort)
            sorting <- order(results$theta)
        
        with(results[sorting,], {
            oldmar <- par("mar") ## adjust the left plot margin
            par(mar=c(oldmar[1], mar_left, oldmar[3], oldmar[4]))
            
            plot(theta, 1:n, ylab = "", xlab = "Theta", xlim = c(min(lower), max(upper)), 
                 type = 'n', yaxt = "n", ...)
            segments(x0 = lower, x1 = upper, y0 = 1:n, y1 = 1:n, col = 'grey', lwd = 2)
            points(theta, 1:n, pch = 20)
            
            axis(2, at = 1:n, labels = doclabels, las = 1)
            par(mar = oldmar) ## put it back again
        })
        
    } else {
        plot(x@beta, x@psi, type = 'n', xlab = "Beta", ylab = "Psi", ...)
        text(x@beta, x@psi, labels = x@features, col = grDevices::rgb(0.7, 0.7, 0.7, alpha))
        
        if (!is.null(highlighted)){
            wch <- x@features %in% highlighted ## will quietly drop highlighteds that do not appear in model
            text(x@beta[wch], x@psi[wch], labels = x@features[wch], col = "black")
        }
    }
}
