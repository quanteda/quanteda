#' Object compilers
#' @rdname object-builder
#' @param source character that indicating source object
#' @param feature character for feature of resulting `dfm`
#' @param docvars data.frame for document level variables
#' @param attrs a list of attributes to be reassigned
#' @param meta list for meta fields
#' @param ... added to object meta fields
#' @keywords internal
build_dfm <- function(x, features,
                        docvars = data.frame(), meta = list(), ...) {
    result <- new("dfm",
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

#' @rdname object-builder
rebuild_dfm <- function(x, attrs) {
    x@meta <- attrs[["meta"]]
    x@docvars <- attrs[["docvars"]]
    x@Dimnames[[1]] <- attrs[["docvars"]][["docname_"]]
    return(x)
}

#' @rdname object-builder
#' @param types character for types of resulting `tokens`` object
#' @param padding logical indicating if the `tokens` object contains paddings
build_tokens <- function(x, types, padding = FALSE,
                           docvars = data.frame(), meta = list(), ...) {
    attributes(x) <- NULL
    structure(x,
              names = docvars[["docname_"]],
              class = "tokens",
              types = types,
              padding = padding,
              docvars = docvars,
              meta = make_meta("tokens", inherit = meta, ...))
}

#' @rdname object-builder
rebuild_tokens <- function(x, attrs) {

    attr(x, "names") <- attrs[["docvars"]][["docname_"]]
    attr(x, "docvars") <- attrs[["docvars"]]
    attr(x, "meta") <- attrs[["meta"]]

    # drop extra attribues for tokens_segment
    try({attr(x, "docnum") <- NULL}, silent = TRUE)
    try({attr(x, "pattern") <- NULL}, silent = TRUE)

    return(x)
}


#' @rdname object-builder
build_corpus <- function(x,
                           docvars = data.frame(),
                           meta = list(), ...) {
    attributes(x) <- NULL
    structure(x,
              names = docvars[["docname_"]],
              class = "corpus",
              docvars = docvars,
              meta = make_meta("corpus", inherit = meta, ...))
}

#' @rdname object-builder
rebuild_corpus <- function(x, attrs) {

    attr(x, "names") <- attrs[["docvars"]][["docname_"]]
    attr(x, "docvars") <- attrs[["docvars"]]
    attr(x, "meta") <- attrs[["meta"]]
    return(x)
}

#' @rdname object-builder
build_dictionary2 <- function(x,
                              meta = list(), ...) {

    new("dictionary2", x,
        meta = make_meta("dictionary2", inherit = meta, ...))

}

#' @rdname object-builder
rebuild_dictionary2 <- function(x, attrs) {

    attr(x, "meta") <- attrs[["meta"]]
    return(x)
}



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

upgrade_dictionary2 <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    build_dictionary2(x,
                      separator = attrs[["concatenator"]],
                      valuetype = "glob")
}
