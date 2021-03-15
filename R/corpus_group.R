#' Combine documents in corpus by a grouping variable
#'
#' Combine documents in a [corpus] object by a grouping variable, by
#' concatenating their texts in the order of the documents within each grouping
#' variable.
#' @param x [corpus] object
#' @param concatenator the concatenation character that will connect the grouped
#'   documents.
#' @inheritParams groups
#' @return a [corpus] object whose documents are equal to
#'   the unique group combinations, and whose texts are the concatenations
#'   of the texts by group. Document-level variables that have no
#'   variation within groups are saved in [docvars].  Document-level
#'   variables that are lists are dropped from grouping, even when these exhibit
#'   no variation within groups.
#'
#'   Setting `fill = TRUE` offers a way to add document groups to the result
#'   that may not have been observed, but for which an empty document is needed,
#'   for various reasons.  If `groups` is a factor of dates, for instance, then
#'   using `fill = TRUE` ensures that the new object will consist of one new
#'   "document" by date, regardless of whether any documents previously existed
#'   with that date.
#' @keywords corpus
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' corpus_group(corp, groups = "grp")
#' corpus_group(corp, groups = c(1, 1, 2, 2))
#' corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3))
corpus_group <- function(x, groups, fill = FALSE, concatenator = " ") {
    UseMethod("corpus_group")
}

#' @export
corpus_group.default <- function(x, groups, fill = FALSE, concatenator = " ") {
    check_class(class(x), "corpus_group")
}

#' @export
corpus_group.corpus <- function(x, groups, fill = FALSE, concatenator = " ") {
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
