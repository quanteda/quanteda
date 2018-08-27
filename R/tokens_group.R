#' Recombine documents tokens by groups
#' 
#' @param x \link{tokens} object
#' @inheritParams groups
#' @keywords tokens internal
#' @examples
#' # dfm_group examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"), 
#'                    docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' toks <- tokens(corp)
#' quanteda:::tokens_group(toks, groups = "grp")
#' quanteda:::tokens_group(toks, groups = c(1, 1, 2, 2))
#' quanteda:::tokens_group(toks, groups = factor(c(1, 1, 2, 2), levels = 1:3))
tokens_group <- function(x, groups = NULL) {
    attrs <- attributes(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups)
    if (length(levels(groups)) > 1) {
        x <- base::split(unlist(unclass(x), use.names = FALSE), rep(groups, lengths(x)))
    } else {
        x <- list(unlist(unclass(x), use.names = FALSE))
        names(x) <- as.character(groups[1])
    }
    x <- structure(x, class = 'tokens')
    docvars(x) <- data.frame(row.names = docnames(x))
    attributes(x, FALSE) <- attrs
    return(x)
}

#' Generate a grouping vector from docvars
#'
#' Internal function to generate a grouping vector from docvars used in
#' dfm.corpus, dfm.tokens, dfm.group, and tokens_group
#' @param x corpus, tokens or dfm
#' @param groups names of docvars or vector that can be coerced to a factor
#' @return a factor
#' @keywords internal
generate_groups <- function(x, groups, drop = FALSE) {
    if (is.character(groups) && all(groups %in% names(documents(x)))) {
        groups <- interaction(documents(x)[, groups], drop = FALSE)
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
        groups <- factor(groups)
    }
    return(groups)
}
