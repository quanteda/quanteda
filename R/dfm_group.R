#' Combine documents in a dfm by a grouping variable
#'
#' Combine documents in a \link{dfm} by a grouping variable, which can also be
#' one of the \link{docvars} attached to the dfm. This is identical in
#' functionality to using the \code{"groups"} argument in \code{\link{dfm}}.
#' @param x a \link{dfm}
#' @inheritParams groups
#' @param fill logical; if \code{TRUE} and \code{groups} is a factor, then use
#'   all levels of the factor when forming the new "documents" of the grouped
#'   dfm.  This will result in documents with zero feature counts for levels not
#'   observed.  Has no effect if the \code{groups} variable(s) are not factors.
#' @return \code{dfm_group} returns a \link{dfm} whose documents are equal to
#'   the unique group combinations, and whose cell values are the sums of the
#'   previous values summed by group. Document-level variables that have no
#'   variation within groups are saved in \link{docvars}.  Document-level
#'   variables that are lists are dropped from grouping, even when these exhibit
#'   no variation within groups.
#'
#'   Setting the \code{fill = TRUE} offers a way to "pad" a dfm with document
#'   groups that may not have been observed, but for which an empty document is
#'   needed, for various reasons.  If \code{groups} is a factor of dates, for
#'   instance, then using \code{fill = TRUE} ensures that the new documents will
#'   consist of one row of the dfm per date, regardless of whether any documents
#'   previously existed with that date.
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                    docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' dfmat <- dfm(corp)
#' dfm_group(dfmat, groups = "grp")
#' dfm_group(dfmat, groups = c(1, 1, 2, 2))
#'
#' # equivalent
#' dfm(dfmat, groups = "grp")
#' dfm(dfmat, groups = c(1, 1, 2, 2))
dfm_group <- function(x, groups = NULL, fill = FALSE) {
    UseMethod("dfm_group")
}

#' @export
dfm_group.default <- function(x, groups = NULL, fill = FALSE) {
    stop(friendly_class_undefined_message(class(x), "dfm_group"))
}
    
#' @export
dfm_group.dfm <- function(x, groups = NULL, fill = FALSE) {

    if (is.null(groups)) return(x)

    x <- as.dfm(x)
    dvars <- docvars_internal(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups)
    if (!fill)
        groups <- droplevels(groups)
    x <- group_dfm(x, documents = groups, fill = fill)
    x <- x[as.character(levels(groups)), ]
    if (length(dvars)) {
        x@docvars <- group_docvars(dvars, groups)
    } else {
        x@docvars <- data.frame(row.names = docnames(x))
    }
    return(x)
}


# ----- internal -------

# internal code to perform dfm compression and grouping
# on features and/or documents
group_dfm <- function(x, features = NULL, documents = NULL, fill = FALSE) {

    if (is.null(features) && is.null(documents))
        return(x)

    temp <- as(x, "dgTMatrix")
    if (is.null(features)) {
        featname <- temp@Dimnames[[2]]
        j_new <- temp@j + 1
    } else {
        featname_unique <- unique(features)
        j <- match(features, featname_unique)
        j_new <- j[temp@j + 1]

        if (!is.factor(features))
            features <- factor(features, levels = featname_unique)
        featname <- as.character(featname_unique)
        if (fill && !identical(levels(features), featname_unique)) {
            featname <- c(featname, setdiff(as.character(levels(features)),
                                            as.character(featname_unique)))
        }
    }
    if (is.null(documents)) {
        docname <- temp@Dimnames[[1]]
        i_new <- temp@i + 1
    } else {
        docname_unique <- unique(documents)
        i <- match(documents, docname_unique)
        i_new <- i[temp@i + 1]

        if (!is.factor(documents))
            documents <- factor(documents, levels = docname_unique)
        docname <- as.character(docname_unique)
        if (fill && !identical(levels(documents), docname_unique)) {
            docname <-
                c(docname, setdiff(as.character(levels(documents)),
                                   as.character(docname_unique)))
        }
    }

    x_new <- temp@x
    dims <- c(length(docname), length(featname))
    result <- new("dfm",
                  sparseMatrix(i = i_new, j = j_new, x = x_new,
                               dims = dims),
                  settings = x@settings,
                  weightTf = x@weightTf,
                  weightDf = x@weightDf,
                  smooth = x@smooth,
                  ngrams = x@ngrams,
                  skip = x@skip,
                  concatenator = x@concatenator)
    set_dfm_dimnames(result) <- list(docname, featname)

    if (is.null(documents)) {
        docvars(result) <- cbind(docvars(x), metadoc(x))
    } else {
        docvars(result) <- data.frame(row.names = docname)
    }
    return(result)
}

# select docvar fields that have all the same values within groups
group_docvars <- function(x, group) {
    result <- x[match(levels(group), group), sapply(x, is_grouped, as.integer(group)), drop = FALSE]
    rownames(result) <- as.character(levels(group))
    return(result)
}

# check if values are uniform within groups
is_grouped <- function(x, group) {
    if (is.list(x)) {
        FALSE
    } else if (is.character(x)) {
        qatd_cpp_is_grouped_character(x, group)
    } else {
        qatd_cpp_is_grouped_numeric(as.numeric(x), group)
    }
}
