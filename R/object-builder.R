# documentation function -------

#' Object builders
#'
#' Functions to build or re-build core objects, or to upgrade earlier versions
#' of these objects to the current format.
#' @name object-builders
#' @param x an input [corpus], [tokens], [dfm], [fcm] or [dictionary] object.
#' @param meta a list for the meta fields (system, object, user). Used to
#'   inherit values in the meta fields from input object. Object meta fields
#'   that are not defined in [quanteda::make_meta()] are ignored with warnings.
#' @param ... values saved in the object meta fields. They overwrite values
#'   passed via `meta`. If not specified, default values in
#'   [quanteda::make_meta()] will be used.
#' @keywords internal
NULL

# dfm --------------

#' @rdname object-builders
#' @param features character for feature of resulting `dfm`.
#' @param docvars data.frame for document level variables created by
#'   [make_docvars()]. Names of documents are extracted from the
#'   `docname_` column.
#' @keywords internal
build_dfm <- function(x, features, # NOTE: consider removing feature
                      docvars = data.frame(), meta = list(), 
                      class = NULL, ...) {
    result <- new("dfm",
                  as(as(as(x, "CsparseMatrix"), "generalMatrix"), "dMatrix"),
                  docvars = docvars,
                  meta = make_meta("dfm", inherit = meta, ...)
    )
    # set names directly to avoid NULL
    result@Dimnames <- list(
        docs = as.character(docvars[["docname_"]]),
        features = as.character(features)
    )
    return(result)
}

#' @rdname object-builders
#' @param attrs a list of attributes to be reassigned
rebuild_dfm <- function(x, attrs) {
    x@meta <- attrs[["meta"]]
    x@docvars <- attrs[["docvars"]]
    x@Dimnames[[1]] <- attrs[["docvars"]][["docname_"]]
    return(x)
}

#' @rdname object-builders
upgrade_dfm <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    if ("meta" %in% names(attrs)) {
        x@docvars <- upgrade_docvars(attrs$docvars, rownames(x))
        return(x)
    }
    
    build_dfm(
        x, colnames(x),
        docvars = upgrade_docvars(attrs$docvars, rownames(x)),
        meta = list(system = list(),
                    object = list(
                        ngram = as.integer(attrs[["ngrams"]]),
                        skip = as.integer(attrs[["skip"]]),
                        concatenator = attrs[["concatenator"]],
                        weight_tf = list(scheme = attrs[["weightTf"]][["scheme"]],
                                         base = attrs[["weightTf"]][["scheme"]],
                                         k = attrs[["weightTf"]][["K"]]),
                        weight_df = list(scheme = attrs[["weightDf"]][["scheme"]],
                                         base = attrs[["weightDf"]][["scheme"]]),
                        smooth = attrs[["smooth"]]
                    ),
                    user = list())
    )

}

# tokens -------

#' @rdname object-builders
#' @param types character for types of resulting the `tokens` object.
#' @param padding logical indicating if the `tokens` object contains paddings.
#' @examples 
#' quanteda:::build_tokens(
#'     list(c(1, 2, 3), c(4, 5, 6)),
#'     docvars = quanteda:::make_docvars(n = 2L),
#'     types = c("a", "b", "c", "d", "e", "f"),
#'     padding = FALSE
#' )
build_tokens <- function(x, types, padding = TRUE,
                         docvars = data.frame(), meta = list(), 
                         class = NULL, ...) {
    
    attributes(x) <- NULL
    class <- setdiff(class, c("tokens_xptr", "tokens")) 
    if (identical(typeof(x), "externalptr")) {
        class <- union(class, c("tokens_xptr", "tokens"))
    } else {
        stopifnot(length(x) == length(docvars[["docname_"]]))
        attr(x, "names") <- docvars[["docname_"]]
        class <- union(class, "tokens")
    }
    structure(x,
              class = class,
              types = types,
              padding = padding, # TODO: removed after v4
              docvars = docvars,
              meta = make_meta("tokens", inherit = meta, ...))
}



#' #' @rdname object-builders
# rebuild_tokens <- function(x, attrs) {
# 
#     attr(x, "docvars") <- attrs[["docvars"]]
#     attr(x, "meta") <- attrs[["meta"]]
#     attr(x, "class") <- union(attrs[["class"]], "tokens")
#     if (is.list(x))
#         attr(x, "names") <- attrs[["docvars"]][["docname_"]]
# 
#     # drop extra attributes from tokens_segment
#     try({attr(x, "docnum") <- NULL}, silent = TRUE)
#     try({attr(x, "pattern") <- NULL}, silent = TRUE)
# 
#     return(x)
# }

#' @rdname object-builders
rebuild_tokens <- function(x, attrs) {

    if (is.list(x))
         attr(x, "names") <- attrs[["docvars"]][["docname_"]]
    structure(x,
              padding = TRUE, # TODO: removed after v4
              docvars = attrs[["docvars"]],
              meta = attrs[["meta"]],
              class = union(attrs[["class"]], "tokens"),
              # drop extra attributes from tokens_segment
              documents = NULL,
              matches = NULL)
}

#' @rdname object-builders
upgrade_tokens <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    if ("meta" %in% names(attrs)) {
        attr(x, "docvars") <- upgrade_docvars(attrs$docvars, names(x))
        return(x)
    }
    
    x <- unclass(x)
    build_tokens(
        x, attrs[["types"]],
        padding = attrs[["padding"]],
        docvars = upgrade_docvars(attr(x, "docvars"), names(x)),
        meta = list(
            system = list(),
            object = list(
                ngram = as.integer(attrs[["ngrams"]]),
                skip = as.integer(attrs[["skip"]]),
                what = attrs[["what"]],
                concatenator = attrs[["concatenator"]]
                ),
            user = list()
        )
    )
}


# corpus --------

#' @rdname object-builders
#' @param class class labels to be attached to the object.
#' @examples 
#' quanteda:::build_corpus(
#'     c("a b c", "d e f"),
#'     docvars = quanteda:::make_docvars(n = 2L),
#'     unit = "sentence"
#' )
build_corpus <- function(x,
                         docvars = data.frame(),
                         meta = list(),
                         class = NULL,
                         ...) {
    
    class <- setdiff(class, c("corpus", "character"))
    stopifnot(length(x) == length(docvars[["docname_"]]))
    attributes(x) <- NULL
    structure(x,
              names = docvars[["docname_"]],
              class = union(class, c("corpus", "character")),
              docvars = docvars,
              meta = make_meta("corpus", inherit = meta, ...))
}

#' @rdname object-builders
rebuild_corpus <- function(x, attrs) {
    attr(x, "names") <- attrs[["docvars"]][["docname_"]]
    attr(x, "docvars") <- attrs[["docvars"]]
    attr(x, "meta") <- attrs[["meta"]]
    attr(x, "class") <- union(attrs[["class"]], c("corpus", "character"))
    return(x)
}

#' @rdname object-builders
upgrade_corpus <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    if ("meta" %in% names(attrs)) {
        attr(x, "docvars") <- upgrade_docvars(attrs$docvars, names(x))
        return(x)
    }
    
    x <- unclass(x)
    if ("documents" %in% names(x)) {
        if ("settings" %in% names(x)) {
            unit <- x[["settings"]][["units"]]
        } else {
            unit <- "documents"
        }
        meta_user <- x[["metadata"]]
        build_corpus(
            x[["documents"]][["texts"]],
            docvars = upgrade_docvars(x[["documents"]]),
            meta = list(system = list(),
                        object = list(unit = unit),
                        user = meta_user[!sapply(meta_user, is.null)])
        )
    } else {
        build_corpus(
            x,
            docvars = attrs[["docvars"]],
            meta = list(system = list(),
                        object = list(unit = attrs[["unit"]]),
                        user = attrs[["meta"]][["user"]])
        )
    }
}


# dictionary ------

#' @rdname object-builders
build_dictionary2 <- function(x, meta = list(), class = "dictionary2", ...) {
    new(class, x,
        meta = make_meta("dictionary2", inherit = meta, ...))

}

#' @rdname object-builders
rebuild_dictionary2 <- function(x, attrs) {
    attr(x, "meta") <- attrs[["meta"]]
    return(x)
}

#' @rdname object-builders
upgrade_dictionary2 <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    build_dictionary2(x,
                      separator = attrs[["concatenator"]],
                      valuetype = "glob")
}

# fcm ------

#' @rdname object-builders
#' @param features1 character for row feature of resulting `fcm`.
#' @param features2 character for column feature of resulting `fcm` iff.
#'   different from `feature1`
#' @param meta list for meta fields
#' @keywords internal
build_fcm <- function(x, features1, features2 = features1,
                      meta = list(), 
                      class = "fcm", ...) {
    result <- new(class,
                  as(as(as(x, "CsparseMatrix"), "generalMatrix"), "dMatrix"),
                  meta = make_meta("fcm", inherit = meta, ...)
    )
    result@Dimnames <- list(
        features = as.character(features1),
        features = as.character(features2)
    )
    return(result)
}

#' @rdname object-builders
#' @param attrs a list of attributes to be reassigned
rebuild_fcm <- function(x, attrs) {
    x@meta <- attrs[["meta"]]
    return(x)
}


#' @rdname object-builders
upgrade_fcm <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    build_fcm(
        x, rownames(x), colnames(x),
        meta = list(system = list(),
                    object = list(
                        concatenator = "_",
                        context = attrs[["context"]], 
                        window = attrs[["window"]], 
                        count = attrs[["count"]], 
                        weights = attrs[["weights"]], 
                        ordered = attrs[["ordered"]], 
                        margin = attrs[["margin"]], 
                        tri = attrs[["tri"]]
                    ),
                    user = list())
    )
}
