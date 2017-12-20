#' @method "[" textstat
#' @export
#' @noRd
"[.textstat" <- function(x, i, j, ...) {
    if (missing(i)) i <- seq_len(nrow(x))
    if (missing(j)) j <- seq_len(ncol(x))
    l <- class(x)
    x <- as.data.frame(x)[i, j, drop = FALSE]
    class(x) <- l
    return(x)
}

#' Subset textstat objects by glob, regex or fixed patterns
#'
#' Users can subset output object of \code{textstat_collocations},
#' \code{textstat_keyness} or \code{textstat_frequency} based on
#' \code{"glob"}, \code{"regex"} or \code{"fixed"} patterns using this method.
#' @method subset textstat
#' @param x a \code{textstat} object
#' @inheritParams pattern
#' @param selection whether to \code{"keep"} or \code{"remove"} the rows that
#'   match the pattern
#' @inheritParams valuetype
#' @param ... additional arguments not used
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @noRd
#' @export
#' @examples
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' result <- textstat_keyness(mydfm)
#' subset(result, pattern = 'america*')
subset.textstat <- function(x, pattern, 
                            selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    
    attrs <- attributes(x)
    id <- unlist(regex2id(pattern, x[[1]], valuetype, case_insensitive))
    if (selection == 'keep') {
        x <- x[id ,]
    } else {
        x <- x[id * -1,]
    }
    class(x) <- attrs$class
    return(x)
}
