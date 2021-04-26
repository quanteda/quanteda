#' Combine documents in a tokens object by a grouping variable
#'
#' Combine documents in a [tokens] object by a grouping variable, by
#' concatenating the tokens in the order of the documents within each grouping
#' variable.
#' @param x [tokens] object
#' @inheritParams groups
#' @return a [tokens] object whose documents are equal to the unique group
#'   combinations, and whose tokens are the concatenations of the tokens by
#'   group. Document-level variables that have no variation within groups are
#'   saved in [docvars].  Document-level variables that are lists are dropped
#'   from grouping, even when these exhibit no variation within groups.
#' @keywords tokens
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' toks <- tokens(corp)
#' tokens_group(toks, groups = grp)
#' tokens_group(toks, groups = c(1, 1, 2, 2))
#'
#' # with fill
#' tokens_group(toks, groups = factor(c(1, 1, 2, 2), levels = 1:3))
#' tokens_group(toks, groups = factor(c(1, 1, 2, 2), levels = 1:3), fill = TRUE)
tokens_group <- function(x, groups = docid(x), fill = FALSE) {
    UseMethod("tokens_group")
}

#' @export
tokens_group.default <- function(x, groups = docid(x), fill = FALSE) {
    check_class(class(x), "tokens_group")
}

#' @export
tokens_group.tokens <- function(x, groups = docid(x), fill = FALSE) {
    x <- as.tokens(x)
    if (missing(groups)) {
        field <- NULL
        groups <- docid(x)
    } else {
        field <- deparse(substitute(groups))
        groups <- eval(substitute(groups), get_docvars(x, user = TRUE, system = TRUE), parent.frame())
        if (!field %in% names(get_docvars(x)) || !is.factor(groups))
            field <- NULL
        groups <- as.factor(groups)
    }
    
    if (!fill)
        groups <- droplevels(groups)
    if (ndoc(x) != length(groups))
        stop("groups must have length ndoc(x)", call. = FALSE)

    # remove NA groups
    x <- tokens_subset(x, !is.na(groups))
    attrs <- attributes(x)
    groups <- groups[!is.na(groups)]

    result <- group_tokens(x, groups)
    attrs[["docvars"]] <- group_docvars(attrs[["docvars"]], groups, field)

    rebuild_tokens(result, attrs)
}

group_tokens <- function(x, groups) {
    result <- split(unlist_integer(unclass(x), use.names = FALSE), rep(groups, lengths(x)))
    attr(result, "class") <- "tokens"
    attr(result, "types") <- attr(x, "types")
    attr(result, "padding") <- attr(x, "padding")
    return(result)
}
