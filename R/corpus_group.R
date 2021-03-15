#' Recombine documents in corpus by a grouping variable
#'
#' Combine documents in a [corpus] object by a grouping variable, which can also
#' be one of the [docvars] attached to the dfm.
#' @param x [corpus] object
#' @param concatenator the concatenation character that will connect the documents.
#' @inheritParams groups
#' @keywords corpus
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' corpus_group(corp, groups = "grp")
#' corpus_group(corp, groups = c(1, 1, 2, 2))
#' corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3))
corpus_group <- function(x, groups = NULL, fill = FALSE, concatenator = " ") {
    UseMethod("corpus_group")
}

#' @export
corpus_group.default <- function(x, groups = NULL, fill = FALSE, concatenator = " ") {
    check_class(class(x), "corpus_group")
}

#' @export
corpus_group <- function(x, groups = NULL, fill = FALSE, concatenator = " ") {
    x <- as.corpus(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups, fill)
    if (!fill)
        groups <- droplevels(groups)
    # remove NA groups
    x <- corpus_subset(x, !is.na(groups))
    attrs <- attributes(x)
    groups <- groups[!is.na(groups)]
    result <- group_corpus(x, groups, concatenator)
    attrs[["docvars"]] <- group_docvars(attrs[["docvars"]], groups)
    rebuild_corpus(result, attrs)
}

group_corpus <- function(x, groups, concatenator = " ") {
    result <- unlist(lapply(split(unclass(x), groups), paste, collapse = concatenator))
    attr(result, "class") <- "corpus"
    return(result)
}
