#' recombine documents tokens by groups
#' 
#' @param x \link{tokens} object
#' @param groups a character or numeric vector that speficy grouping of documents
tokens_group <- function(x, groups) {
    
    attrs <- attributes(x)
    groups <- as.character(groups)
    groups_index <- rep(groups, lengths(x))
    x <- base::split(unlist(unclass(x), use.names = FALSE), factor(groups_index, levels = unique(groups)))
    names(x) <- as.character(names(x))
    attributes(x, FALSE) <- attrs
    docvars(x) <- NULL
    return(x)
}