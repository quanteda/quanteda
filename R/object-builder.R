# documentation function -------

#' Object compilers
#' 
#' Functions to build or re-build core objects, or to upgrade earlier versions
#' of these objects to the current format.
#' @name object-builders
#' @param x an input [corpus], [dfm], [tokens], or [dictionary] object
#' @param ... further objects passed to object metadata
#' @keywords internal
NULL

# dfm --------------

#' @rdname object-builders
#' @param features character for feature of resulting `dfm`
#' @param docvars data.frame for document level variables
#' @param meta list for meta fields
#' @keywords internal
build_dfm <- function(x, features,
                      docvars = data.frame(), meta = list(), 
                      class = "dfm", ...) {
    result <- new(class,
        as(x, "dgCMatrix"),
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
#' @param types character for types of resulting `tokens`` object
#' @param padding logical indicating if the `tokens` object contains paddings
build_tokens <- function(x, types, padding = FALSE,
                         docvars = data.frame(), meta = list(), 
                         class = "tokens", ...) {
    stopifnot(length(x) == length(docvars[["docname_"]]))
    attributes(x) <- NULL
    structure(x,
              names = docvars[["docname_"]],
              class = union(class, "tokens"),
              types = types,
              padding = padding,
              docvars = docvars,
              meta = make_meta("tokens", inherit = meta, ...))
}

#' @rdname object-builders
rebuild_tokens <- function(x, attrs) {
    attr(x, "names") <- attrs[["docvars"]][["docname_"]]
    attr(x, "docvars") <- attrs[["docvars"]]
    attr(x, "meta") <- attrs[["meta"]]
    attr(x, "class") <- union(attrs[["class"]], "tokens")

    # drop extra attribues for tokens_segment
    try({attr(x, "docnum") <- NULL}, silent = TRUE)
    try({attr(x, "pattern") <- NULL}, silent = TRUE)

    return(x)
}

#' @rdname object-builders
upgrade_tokens <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
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
#' @param class class to be attached to the built object
build_corpus <- function(x,
                         docvars = data.frame(),
                         meta = list(),
                         class = "corpus",
                         ...) {
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
    attr(x, "class") <- union(attrs[["class"]], "dictionary2")
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
