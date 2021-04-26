#' Combine documents in corpus by a grouping variable
#'
#' Combine documents in a [corpus] object by a grouping variable, by
#' concatenating their texts in the order of the documents within each grouping
#' variable.
#' @param x [corpus] object
#' @param concatenator the concatenation character that will connect the grouped
#'   documents.
#' @inheritParams groups
#' @return a [corpus] object whose documents are equal to the unique group
#'   combinations, and whose texts are the concatenations of the texts by group.
#'   Document-level variables that have no variation within groups are saved in
#'   [docvars].  Document-level variables that are lists are dropped from
#'   grouping, even when these exhibit no variation within groups.
#' @keywords corpus
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' corpus_group(corp, groups = grp)
#' corpus_group(corp, groups = c(1, 1, 2, 2))
#' corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3))
#' 
#' # with fill
#' corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3), fill = TRUE)
corpus_group <- function(x, groups = docid(x), fill = FALSE, concatenator = " ") {
    UseMethod("corpus_group")
}

#' @export
corpus_group.default <- function(x, groups = docid(x), fill = FALSE, concatenator = " ") {
    check_class(class(x), "corpus_group")
}

#' @export
corpus_group.corpus <- function(x, groups = docid(x), fill = FALSE, concatenator = " ") {
    x <- as.corpus(x)
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
    x <- corpus_subset(x, !is.na(groups))
    attrs <- attributes(x)
    groups <- groups[!is.na(groups)]
    
    result <- group_corpus(x, groups, concatenator)
    attrs[["docvars"]] <- group_docvars(attrs[["docvars"]], groups, field)
    
    rebuild_corpus(result, attrs)
}

group_corpus <- function(x, groups, concatenator = " ") {
    result <- unlist_character(lapply(split(unclass(x), groups), paste, collapse = concatenator))
    attr(result, "class") <- "corpus"
    return(result)
}
