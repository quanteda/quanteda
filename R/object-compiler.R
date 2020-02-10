#' Object compilers
#' @rdname object-compiler
#' @param source character that indicating source object
#' @param feature character for feature of resulting `dfm`
#' @param docvars data.frame for docment level variables 
#' @param meta list for meta fields
#' @param ... added to object meta fields  
#' @keywords internal
compile_dfm <- function(x, features,
                        docvars = data.frame(), meta = list(), ...) {
    new("dfm", 
        as(x, "dgCMatrix"),
        Dimnames = list(
              docs = as.character(docvars[["docname_"]]), 
              features = as.character(features)
        ),
        docvars = docvars,
        meta = make_meta("dfm", inherit = meta, ...)
    )
}

#' @rdname object-compiler
#' @param types character for types of resulting `tokens`` object
#' @param padding logical indicating if the `tokens` object contains paddings
compile_tokens <- function(x, types, padding = FALSE,
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

#' @rdname object-compiler
compile_corpus <- function(x, 
                           docvars = data.frame(), 
                           meta = list(), ...) {
    attributes(x) <- NULL
    structure(x,
              names = docvars[["docname_"]],
              class = "corpus",
              docvars = docvars,
              meta = make_meta("corpus", inherit = meta, ...))
}


upgrade_dfm <- function(x) {
    if (!is_pre2(x)) return(x)
    attrs <- attributes(x)
    compile_dfm(
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
    compile_tokens(
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
    x <- unclass(x)
    if ("documents" %in% names(x)) {
        if ("settings" %in% names(x)) {
            unit <- x[["settings"]][["units"]]
            x[["settings"]][["unit"]] <- NULL
        } else {
            unit <- "documents"
        }
        
        if ("metadata" %in% names(x)) {
            created <- as.POSIXct(
                x[["metadata"]][["created"]],
                format = "%a %b %d %H:%M:%S %Y"
            )
            x[["metadata"]][["created"]] <- NULL
        } else {
            created <- Sys.Date()
        }
        meta_user <- x[["metadata"]]
        compile_corpus(
            x[["documents"]][["texts"]], 
            docvars = upgrade_docvars(x[["documents"]]),
            meta = list(system = list(created = created),
                        object = list(unit = unit),
                        user = meta_user[!sapply(meta_user, is.null)])
        )
    } else {
        attrs <- attributes(x)
        compile_corpus(
            x, 
            docvars = attrs[["docvars"]],
            meta = list(system = attrs[["meta"]][["system"]],
                        object = list(unit = attrs[["unit"]]),
                        user = attrs[["meta"]][["user"]])
        )
        
    }
}
