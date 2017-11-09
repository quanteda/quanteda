#' recombine documents tokens by groups
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
tokens_group <- function(x, groups = NULL) {
    attrs <- attributes(x)
    groups <- generate_groups(x, groups)
    groups_unique <- unique(groups)
    if (length(groups_unique) > 1) {
        x <- base::split(unlist(unclass(x), use.names = FALSE), 
                         rep(factor(groups, levels = groups_unique), lengths(x)))
    } else {
        x <- list(unlist(unclass(x), use.names = FALSE))
        names(x) <- as.character(groups[1])
    }
    x <- structure(x, class = 'tokens')
    docvars(x) <- data.frame(row.names = docnames(x))
    attributes(x, FALSE) <- attrs
    return(x)
}

# internal function to generate a grouping vector from docvars
# used in dfm.corpus, dfm.tokens, tokens_group
generate_groups <- function(x, groups) {
    if (is.character(groups) && all(groups %in% names(documents(x)))) {
        groups <- interaction(documents(x)[, groups], drop = TRUE)
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
    }
    return(groups)
}
