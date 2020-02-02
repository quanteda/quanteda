#' Combine documents in a dfm by a grouping variable
#'
#' Combine documents in a [dfm] by a grouping variable, which can also be
#' one of the [docvars] attached to the dfm. This is identical in
#' functionality to using the `"groups"` argument in [dfm()].
#' @param x a [dfm]
#' @inheritParams groups
#' @param force logical; if `TRUE`, group by summing existing counts, even
#'   if the dfm has been weighted.  This can result in invalid sums, such as
#'   adding log counts (when a dfm has been weighted by `"logcount"` for
#'   instance using [dfm_weight()]).  Does not apply to the term
#'   weight schemes "count" and "prop".
#' @param fill logical; if `TRUE` and `groups` is a factor, then use
#'   all levels of the factor when forming the new "documents" of the grouped
#'   dfm.  This will result in documents with zero feature counts for levels not
#'   observed.  Has no effect if the `groups` variable(s) are not factors.
#' @return `dfm_group` returns a [dfm] whose documents are equal to
#'   the unique group combinations, and whose cell values are the sums of the
#'   previous values summed by group. Document-level variables that have no
#'   variation within groups are saved in [docvars].  Document-level
#'   variables that are lists are dropped from grouping, even when these exhibit
#'   no variation within groups.
#'
#'   Setting the `fill = TRUE` offers a way to "pad" a dfm with document
#'   groups that may not have been observed, but for which an empty document is
#'   needed, for various reasons.  If `groups` is a factor of dates, for
#'   instance, then using `fill = TRUE` ensures that the new documents will
#'   consist of one row of the dfm per date, regardless of whether any documents
#'   previously existed with that date.
#' @export
#' @examples
#' corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
#'                docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' dfmat <- dfm(corp)
#' dfm_group(dfmat, groups = "grp")
#' dfm_group(dfmat, groups = c(1, 1, 2, 2))
#'
#' # equivalent
#' dfm(dfmat, groups = "grp")
#' dfm(dfmat, groups = c(1, 1, 2, 2))
dfm_group <- function(x, groups = NULL, fill = FALSE, force = FALSE) {
    UseMethod("dfm_group")
}

#' @export
dfm_group.default <- function(x, groups = NULL, fill = FALSE, force = FALSE) {
    stop(friendly_class_undefined_message(class(x), "dfm_group"))
}
    
#' @export
dfm_group.dfm <- function(x, groups = NULL, fill = FALSE, force = FALSE) {
    
    x <- as.dfm(x)
    attrs <- attributes(x)
    
    if (is.null(groups))
        groups <- docid(x)

    if (!force) {
        if ((!field_object(attrs, "weight_tf")[["scheme"]] %in% c("count", "prop") &&
            field_object(attrs, "weight_df")[["scheme"]] != "unary") ||
            field_object(attrs, "weight_df")[["scheme"]] != "unary") {
            stop("will not group a weighted dfm; use force = TRUE to override",
                 call. = FALSE)
        }
    }
    if (!nfeat(x) || !ndoc(x)) return(x)
    if (!is.factor(groups))
        groups <- generate_groups(x, groups)
    if (!fill)
        groups <- droplevels(groups)
    x <- group_dfm(x, documents = groups, fill = fill)
    x <- x[levels(groups),]
    return(x)
}


#' Generate a grouping vector from docvars
#'
#' Internal function to generate a grouping vector from docvars used in
#' dfm.corpus, dfm.tokens, dfm.group, and tokens_group
#' @param x corpus, tokens or dfm
#' @param groups names of docvars or vector that can be coerced to a factor
#' @return a factor
#' @keywords internal
generate_groups <- function(x, groups, drop = FALSE) {
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    if (is.character(groups) && all(groups %in% names(docvar))) {
        groups <- interaction(docvar[groups], drop = FALSE)
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
        groups <- factor(groups)
    }
    return(groups)
}


# select docvar fields that have all the same values within groups
# group_docvars <- function(x, group) {
#     result <- x[match(levels(group), group), sapply(x, is_grouped, as.integer(group)), drop = FALSE]
#     rownames(result) <- as.character(levels(group))
#     return(result)
# }

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

# internal code to perform dfm compression and grouping
# on features and/or documents
group_dfm <- function(x, documents = NULL, features = NULL, fill = FALSE, use_docvars = TRUE) {

    if (!length(features) && !length(documents))
        return(x)
    temp <- as(x, "dgTMatrix")
    if (is.null(features)) {
        featname <- temp@Dimnames[[2]]
        j_new <- temp@j + 1L
    } else {
        if (!is.factor(features))
            features <- factor(features, levels = unique(features))
        if (!fill)
            features <- droplevels(features)
        featname <- levels(features)
        j <- as.integer(features)
        j_new <- j[temp@j + 1L]
    }
    if (is.null(documents)) {
        docname <- temp@Dimnames[[1]]
        i_new <- temp@i + 1L
    } else {
        if (!is.factor(documents))
            documents <- factor(documents, levels = unique(features))
        if (!fill)
            documents <- droplevels(documents)
        docname <- levels(documents)
        i <- as.integer(documents)
        i_new <- i[temp@i + 1L]
    }
    x_new <- temp@x
    
    if (use_docvars) {
        if (is.null(documents)) {
            docvars <- x@docvars
        } else {
            docvars <- group_docvars(x@docvars, documents)
        }
    } else {
        docvars <- make_docvars(length(docname), docname)
    }
    
    temp <- new("dfm",
                sparseMatrix(i = i_new, j = j_new, x = x_new,
                             dims = c(length(docname), length(featname))))
    compile_dfm(temp, 
                source = "dfm",
                features = featname,
                docvars = docvars,
                meta = meta(x, type = "all")
    )
    # 
    # x_new <- temp@x
    # dims <- c(length(docname), length(featname))
    # result <- new("dfm",
    #               sparseMatrix(i = i_new, j = j_new, x = x_new,
    #                            dims = dims),
    #               weightTf = x@weightTf,
    #               weightDf = x@weightDf,
    #               smooth = x@smooth,
    #               ngrams = x@ngrams,
    #               skip = x@skip,
    #               meta = x@meta,
    #               concatenator = x@concatenator)
    # set_dfm_dimnames(result) <- list(docname, featname)
    # 
    # if (use_docvars) {
    #     if (is.null(documents)) {
    #         result@docvars <- x@docvars
    #     } else {
    #         result@docvars <- group_docvars(x@docvars, documents)
    #     }
    # } else {
    #     result@docvars <- data.frame()
    # }
    # return(result)
}
