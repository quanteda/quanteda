#' Recombine documents tokens by groups
#' 
#' @param x [tokens] object
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
tokens_group <- function(x, groups = NULL, fill = FALSE) {
    
    x <- as.tokens(x)
    attrs <- attributes(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups, fill)
    if (!fill)
        groups <- droplevels(groups)
    result <- base::split(unlist(unclass(x), use.names = FALSE), rep(groups, lengths(x)))
    docvars <- group_docvars(attrs[["docvars"]], groups)
    compile_tokens(
        result, "tokens", docvars[["docname_"]], 
        types = attrs[["types"]], 
        padding = attrs[["padding"]], 
        docvars = docvars,
        meta = meta(x, type = "all")
    )
}
