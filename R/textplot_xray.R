#' Plot the dispersion of key word(s)
#' 
#' Plots a dispersion or "x-ray" plot of selected word pattern(s) across one or 
#' more texts. The format of the plot depends on the number of [kwic] class
#' objects passed: if there is only one document, keywords are plotted one below
#' the other. If there are multiple documents the documents are plotted one 
#' below the other, with keywords shown side-by-side. Given that this returns a 
#' \pkg{ggplot2} object, you can modify the plot by adding \pkg{ggplot2} layers
#' (see example).
#' @param ... any number of [kwic] class objects
#' @param scale whether to scale the token index axis by absolute position of
#'   the token in the document or by relative position. Defaults are absolute
#'   for single document and relative for multiple documents.
#' @param sort whether to sort the rows of a multiple document plot by document
#'   name
#' @return a \pkg{ggplot2} object
#' @section Known Issues:
#' These are known issues on which we are working to solve in future versions:
#' * `textplot_xray()` will not display the patterns correctly when
#'   these are multi-token sequences.
#' * For dictionaries with keys that have overlapping value matches to tokens in
#'   the text, only the first match will be used in the plot.  The way around this
#'   is to produce one kwic per dictionary key, and send them as a list to
#'   `textplot_xray`.
#' @examples 
#' \dontrun{
#' corp <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' # compare multiple documents
#' textplot_xray(kwic(corp, pattern = "american"))
#' textplot_xray(kwic(corp, pattern = "american"), scale = "absolute")
#' 
#' # compare multiple terms across multiple documents
#' textplot_xray(kwic(corp, pattern = "america*"), 
#'               kwic(corp, pattern = "people"))
#' 
#' # how to modify the ggplot with different options
#' library(ggplot2)
#' tplot <- textplot_xray(kwic(corp, pattern = "american"), 
#'                        kwic(corp, pattern = "people"))
#' tplot + aes(color = keyword) + scale_color_manual(values = c('red', 'blue'))
#' 
#' # adjust the names of the document names
#' docnames(corp) <- apply(docvars(corp, c("Year", "President")), 1, paste, collapse = ", ")
#' textplot_xray(kwic(corp, pattern = "america*"), 
#'               kwic(corp, pattern = "people"))
#' }
#' @export
#' @keywords textplot
textplot_xray <- function(..., scale = c("absolute", "relative"),
                          sort = FALSE) {
    UseMethod("textplot_xray")
}
    
#' @export
textplot_xray.default <- function(..., scale = c("absolute", "relative"),
                                  sort = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textplot_xray"))
}

#' @importFrom data.table :=
#' @export
textplot_xray.kwic <- function(..., scale = c("absolute", "relative"),
                               sort = FALSE) {

    if (!requireNamespace("ggplot2", quietly = TRUE))
        stop("You must have ggplot2 installed to make a dispersion plot.")
    if (!requireNamespace("grid", quietly = TRUE))
        stop("You must have grid installed to make a dispersion plot.")

    position <- from <- keyword <- docname <- ntokens <- NULL

    kwics <- list(...)
    if (!all(vapply(kwics, is.kwic, logical(1))))
        stop("objects to plot must be kwic objects")
    
    # create a data.table from the kwic arguments
    x <- data.table(do.call(rbind, kwics))
    # use old variable name
    x[, position := from]
    # get the vector of ntokens
    ntokensbydoc <- unlist(lapply(kwics, attr, "ntoken"))
    # add ntokens to data.table as an indexed "merge"
    x[, ntokens := ntokensbydoc[as.character(x[, docname])]]

    # replace "found" keyword with patterned keyword
    x[, keyword := unlist(lapply(kwics, function(l) l[["pattern"]]))]

    # pre-emptively convert keyword to factor before ggplot does it, so that we
    # can keep the order of the factor the same as the order of the kwic objects
    # x[, keyword := factor(keyword, levels = unique(keyword))]

    multiple_documents <- length(unique(x$docname)) > 1

    # Deal with the scale argument:
    # if there is a user-supplied value, use that after passing through
    # match.argj; if not, use relative for multiple documents and absolute
    # for single documents
    if (!missing(scale)) {
        scale <- match.arg(scale)
    }
    else {
        if (multiple_documents) {
            scale <- "relative"
        } else {
            scale <- "absolute"
        }
    }

    # Deal with the sort argument:
    if (sort) {
        x[, docname := factor(docname)] # levels are sorted by default
    } else {
        x[, docname := factor(docname, levels = unique(docname))]
    }

    if (scale == "relative")
        x[, position := position / ntokens]

    plot <- ggplot2::ggplot(x, ggplot2::aes(x = position, y = 1)) +
        ggplot2::geom_segment(ggplot2::aes(xend = position, yend = 0)) +
        ggplot2::theme(axis.line = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       panel.spacing = grid::unit(0.1, "lines"),
                       panel.border = ggplot2::element_rect(colour = "gray", fill = NA),
                       strip.text.y = ggplot2::element_text(angle = 0)
        )

    if (scale == "absolute")
        plot <- plot +
          ggplot2::geom_rect(ggplot2::aes(xmin = ntokens, xmax = max(x$ntokens),
                                          ymin = 0, ymax = 1), fill = "gray90")

    if (multiple_documents) {
        # If there is more than one document, put documents on the panel y-axis
        # and keyword(s) on the panel x-axis
        plot <- plot + ggplot2::facet_grid(docname ~ keyword) +
            ggplot2::labs(y = "Document", title = paste("Lexical dispersion plot"))
    }
    else {
        # If not, put keywords on the panel y-axis and the doc name in the title
        plot <- plot + ggplot2::facet_grid(keyword~.) +
            ggplot2::labs(y = "", title = paste("Lexical dispersion plot, document:",
                                                x$docname[[1]]))
    }

    if (scale == "relative") {
        plot <- plot + ggplot2::labs(x = "Relative token index")
    }
    else {
        plot <- plot + ggplot2::labs(x = "Token index")
    }

    plot
}
