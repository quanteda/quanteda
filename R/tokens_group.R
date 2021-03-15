#' Recombine documents in tokens by a grouping variable
#'
#' Combine documents in a [tokens] object by a grouping variable, which can also
#' be one of the [docvars] attached to the dfm.
#' @param x [tokens] object
#' @inheritParams groups
#' @keywords tokens
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' toks <- tokens(corp)
#' tokens_group(toks, groups = "grp")
#' tokens_group(toks, groups = c(1, 1, 2, 2))
#' tokens_group(toks, groups = factor(c(1, 1, 2, 2), levels = 1:3))
tokens_group <- function(x, groups = NULL, fill = FALSE) {
    UseMethod("tokens_group")
}

#' @export
tokens_group.default <- function(x, groups = NULL, fill = FALSE) {
    check_class(class(x), "tokens_group")
}

#' @export
tokens_group.tokens <- function(x, groups = NULL, fill = FALSE) {
    x <- as.tokens(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups, fill)
    if (!fill)
        groups <- droplevels(groups)
    # remove NA groups
    x <- tokens_subset(x, !is.na(groups))
    attrs <- attributes(x)
    groups <- groups[!is.na(groups)]
    result <- group_tokens(x, groups)
    attrs[["docvars"]] <- group_docvars(attrs[["docvars"]], groups)
    rebuild_tokens(result, attrs)
}

group_tokens <- function(x, groups) {
    result <- split(unlist(unclass(x), use.names = FALSE), rep(groups, lengths(x)))
    attr(result, "class") <- "tokens"
    attr(result, "types") <- attr(x, "types")
    attr(result, "padding") <- attr(x, "padding")
    return(result)
}
