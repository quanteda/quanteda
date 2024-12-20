#' Combine documents in a dfm by a grouping variable
#'
#' Combine documents in a [dfm] by a grouping variable, by summing the cell
#' frequencies within group and creating new "documents" with the group labels.
#' @param x a [dfm]
#' @inheritParams groups
#' @inheritParams messages
#' @param force logical; if `TRUE`, group by summing existing counts, even if
#'   the dfm has been weighted.  This can result in invalid sums, such as adding
#'   log counts (when a dfm has been weighted by `"logcount"` for instance using
#'   [dfm_weight()]).  Not needed when the term weight schemes "count" and
#'   "prop".
#' @return `dfm_group` returns a [dfm] whose documents are equal to
#'   the unique group combinations, and whose cell values are the sums of the
#'   previous values summed by group. Document-level variables that have no
#'   variation within groups are saved in [docvars].  Document-level
#'   variables that are lists are dropped from grouping, even when these exhibit
#'   no variation within groups.
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' dfmat <- dfm(tokens(corp))
#' dfm_group(dfmat, groups = grp)
#' dfm_group(dfmat, groups = c(1, 1, 2, 2))
#'
#' # with fill = TRUE
#' dfm_group(dfmat, fill = TRUE,
#'           groups = factor(c("A", "A", "B", "C"), levels = LETTERS[1:4]))
dfm_group <- function(x, groups = docid(x), fill = FALSE, force = FALSE,
                      verbose = quanteda_options("verbose")) {
    UseMethod("dfm_group")
}

#' @export
dfm_group.default <- function(x, groups, fill = FALSE, force = FALSE,
                              verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_group")
}
    
#' @export
dfm_group.dfm <- function(x, groups = docid(x), fill = FALSE, force = FALSE,
                          verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    fill <- check_logical(fill)
    force <- check_logical(force)

    attrs <- attributes(x)
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

    if (!force) {
        if ((!field_object(attrs, "weight_tf")[["scheme"]] %in% c("count", "prop") &&
            field_object(attrs, "weight_df")[["scheme"]] != "unary") ||
            field_object(attrs, "weight_df")[["scheme"]] != "unary") {
            stop("will not group a weighted dfm; use force = TRUE to override",
                 call. = FALSE)
        }
    }
    if (!ndoc(x)) return(x)
    if (!fill)
        groups <- droplevels(groups)
    if (ndoc(x) != length(groups))
        stop("groups must have length ndoc(x)", call. = FALSE)

    if (verbose)
        before <- stats_dfm(x)
    
    # remove NA groups
    x <- dfm_subset(x, !is.na(groups))
    attrs <- attributes(x)
    groups <- groups[!is.na(groups)]
    
    x <- group_matrix(x, documents = groups, fill = fill)
    result <- build_dfm(x, colnames(x),
                        unit = "documents",
                        docvars = group_docvars(attrs[["docvars"]], groups, field),
                        meta = attrs[["meta"]]
    )
    if (verbose)
        message_dfm("dfm_group()", before, stats_dfm(result))
    return(result)
}

# check if values are uniform within groups
is_grouped <- function(x, group) {
    if (is.list(x)) {
        FALSE
    } else if (is.character(x)) {
        cpp_is_grouped_character(x, group)
    } else {
        cpp_is_grouped_numeric(as.numeric(x), group)
    }
}

# internal code to perform sparse matrix compression and grouping
# on features and/or documents
group_matrix <- function(x, documents = NULL, features = NULL, fill = FALSE) {

    if (!length(features) && !length(documents))
        return(x)
    attrs <- attributes(x)
    x <- as(x, "TsparseMatrix")
    if (is.null(features)) {
        featname <- x@Dimnames[[2]]
        j <- x@j + 1L
    } else {
        if (!is.factor(features))
            features <- factor(features, levels = unique(features))
        if (!fill)
            features <- droplevels(features)
        featname <- levels(features)
        j <- as.integer(features)
        j <- j[x@j + 1L]
    }
    if (is.null(documents)) {
        docname <- x@Dimnames[[1]]
        i <- x@i + 1L
    } else {
        if (!is.factor(documents))
            documents <- factor(documents, levels = unique(documents))
        if (!fill)
            documents <- droplevels(documents)
        docname <- levels(documents)
        i <- as.integer(documents)
        i <- i[x@i + 1L]
    }
    sparseMatrix(i = i, j = j, x = x@x, 
                 dims = c(length(docname), length(featname)),
                 dimnames = list(docname, featname))
}
