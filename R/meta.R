# meta ---------------------

#' Get or set object metadata
#'
#' Get or set the object metadata in a [corpus], [tokens], [dfm], or
#' [dictionary] object. With the exception of dictionaries, this will be
#' corpus-level metadata.
#'
#' `metacorpus` and `metacorpus<-` are synonyms but are deprecated.
#' @param x an object for which the metadata will be read or set
#' @param field metadata field name(s); if `NULL` (default), return all
#'   metadata names
#' @param type `"user"` for user-provided corpus-level metadata;
#'   `"system"` for metadata set automatically when the corpus is created;
#'   or `"all"` for all metadata.
#' @return For `meta`, a named list of the metadata fields in the corpus.
#'
#'   For `meta <-`, the corpus with the updated user-level metadata.  Only
#'   user-level metadata may be assigned.
#' @export
#' @keywords corpus
#' @aliases metacorpus
#' @examples
#' meta(data_corpus_inaugural)
#' meta(data_corpus_inaugural, "source")
#' meta(data_corpus_inaugural, "citation") <- "Presidential Speeches Online Project (2014)."
#' meta(data_corpus_inaugural, "citation")
meta <- function(x, field = NULL, type = c("user", "object", "system", "all"))
    UseMethod("meta")

#' @export
meta.default <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    stop(friendly_class_undefined_message(class(x), "meta"))
}

#' @export
meta.corpus <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x) && "metadata" %in% names(x))
        return(x[["metadata"]])
    select_meta(attr(x, "meta"), field, type)
}

#' @export
meta.tokens <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x)) return(NULL)
    select_meta(attr(x, "meta"), field, type)
}

#' @export
meta.dfm <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x)) return(NULL)
    select_meta(x@meta, field, type)
}

#' @export
meta.dictionary2 <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x)) return(NULL)
    select_meta(x@meta, field, type)
}

select_meta <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    type <- match.arg(type)
    if (type == "all")
        return(x)
    if (is.null(field)) {
        return(x[[type]])
    } else {
        return(x[[type]][[field]])
    }
}

# meta<-   -----------

#' Replacement function for corpus-level data
#' @param value new value of the metadata field
#' @export
#' @rdname meta
#' @aliases "metacorpus<-"
"meta<-" <- "metacorpus<-" <- function(x, field = NULL, value) {
    if (is.null(field) && !is.list(value)) stop("value must be a named list")
        if (is.list(value) && length(names(value)) != length(value))
        stop("every element of the meta list must be named")
    UseMethod("meta<-")
}

#' @export
`meta<-.corpus` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        attr(x, "meta")$user <- value
    } else {
        attr(x, "meta")$user[[field]] <- value
    }
    return(x)
}

#' @export
`meta<-.tokens` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        attr(x, "meta")$user <- value
    } else {
        attr(x, "meta")$user[[field]] <- value
    }
    return(x)
}


#' @export
`meta<-.dfm` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x@meta$user <- value
    } else {
        x@meta$user[[field]] <- value
    }
    return(x)
}

#' @export
`meta<-.dictionary2` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x@meta$user <- value
    } else {
        x@meta$user[[field]] <- value
    }
    return(x)
}


# legacy functions ----------

#' @rdname meta
#' @export
metacorpus <- meta

#' @rdname meta
#' @export
`metacorpus<-` <- `meta<-`

# internal: meta_system ----------------

#' Internal function to get, set or initialize system metadata
#'
#' Sets or initializes system metadata for new objects.
#' @inheritParams meta
#' @return `meta_system` returns a list with the object's system metadata.
#'   It is literally a wrapper to [`meta(x, field, type =
#'   "system")()`][meta].
#' @keywords internal
#' @examples
#' corp <- corpus(c(d1 = "one two three", d2 = "two three four"))
#' # quanteda:::`meta_system<-`(corp, value = quanteda:::meta_system_defaults("example"))
#' quanteda:::meta_system(corp)
meta_system <- function(x, field = NULL)
    meta(x, field = field, type = "system")

#' @rdname meta_system
#' @return `meta_system<-` returns the object with the system metadata
#'   modified. This is an internal function and not designed for users!
`meta_system<-` <- function(x, field = NULL, value) {
    if (is.null(field) && !is.list(value)) stop("value must be a named list")
    if (is.list(value) && length(names(value)) != length(value))
        stop("every element of the meta list must be named")
    UseMethod("meta_system<-")
}

#' @rdname meta_system
`meta_system<-.corpus` <- function(x, field = NULL, value) {
    if (is.null(field) && !missing(value)) {
        attr(x, "meta")$system <- value
    } else {
        attr(x, "meta")$system[[field]] <- value
    }
    return(x)
}

#' @rdname meta_system
`meta_system<-.tokens` <- function(x, field = NULL, value) {
    if (is.null(field) && !missing(value)) {
        attr(x, "meta")$system <- value
    } else {
        attr(x, "meta")$system[[field]] <- value
    }
    return(x)
}


#' @rdname meta_system
`meta_system<-.dfm` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x@meta$system <- value
    } else {
        x@meta$system[[field]] <- value
    }
    return(x)
}

#' @rdname meta_system
`meta_system<-.dictionary` <- `meta_system<-.dfm`

#' @rdname meta_system
#' @param source character; the input object class
#' @return `meta_system_defaults` returns a list of default system
#'   values, with the user setting the "source" value.  This should be used
#'   to set initial system meta information.
meta_system_defaults <- function() {
   list("package-version" = utils::packageVersion("quanteda"),
        "r-version" = getRversion(),
        "system" = Sys.info()[c("sysname", "machine", "user")],
        "directory" = getwd(),
        "created" = Sys.Date()
   )
}

# make_meta ------------

#' Internal functions to create a list for the meta attribute
#' @param class object class either `dfm`, `tokens` or `corpus`
#' @param inherit list from the meta attribute
#' @param ... values assigned to the object meta fields
#' @keywords internal
make_meta <- function(class, inherit = NULL, ...) {

    if (is.null(inherit))
        inherit <- list()

    result <- list(
        "system" = list(),
        "object" = list(),
        "user" = list()
    )

    suppressWarnings({
        result$system <- make_meta_system(inherit$system)
    })
    if (class == "corpus") {
        result$object <- make_meta_corpus(inherit$object, ...)
    } else if (class == "tokens") {
        result$object <- make_meta_tokens(inherit$object, ...)
    } else if (class == "dfm") {
        result$object <- make_meta_dfm(inherit$object, ...)
    } else if (class == "dictionary2") {
        result$object <- make_meta_dictionary2(inherit, ...)
    }

    if ("user" %in% names(inherit))
        result$user <- inherit$user

    return(result)
}

# newer version of meta_system_defaults
make_meta_system <- function(inherit = NULL) {
    if (is.null(inherit))
        inherit <- list()
    default <- list(
        "package-version" = utils::packageVersion("quanteda"),
        "r-version" = getRversion(),
        "system" = Sys.info()[c("sysname", "machine", "user")],
        "directory" = getwd(),
        "created" = Sys.Date()
    )
    update_meta(default, inherit)
}

#' @rdname make_meta
make_meta_corpus <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list("unit" = "documents",
                    "summary" = list("hash" = character(), 
                                     "data" = NULL)
                    )
    update_meta(default, inherit, ...)
}

#' @rdname make_meta
make_meta_tokens <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list(
        "unit" = "documents",
        "what" = "word",
        "ngram" = 1L,
        "skip" = 0L,
        "concatenator" = "_",
        "summary" = list("hash" = character(), 
                         "data" = NULL)
    )
    update_meta(default, inherit, ...)
}

#' @rdname make_meta
make_meta_dfm <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list(
        "unit" = "documents",
        "what" = "word",
        "ngram" = 1L,
        "skip" = 0L,
        "concatenator" = "_",
        "weight_tf" = list(scheme = "count", base = NULL, k = NULL),
        "weight_df" = list(scheme = "unary", base = NULL, c = NULL,
                           smoothing = NULL, threshold = NULL),
        "smooth" = 0,
        "summary" = list("hash" = character(), 
                         "data" = NULL)
    )
    update_meta(default, inherit, ...)
}

#' @rdname make_meta
make_meta_dictionary2 <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list("valuetype" = "glob",
                    "separator" = " ")
    update_meta(default, inherit, ...)
}

#' @rdname make_meta
#' @param default default values for the meta attribute
update_meta <- function(default, inherit, ...) {
    update <- list(...)
    for (m in setdiff(union(names(inherit), names(update)), names(default))) {
        warning(m, " is ignored.")
    }
    for (m in names(default)) {
        if (length(update) && m %in% names(update)) {
            if (!identical(class(default[[m]]), class(update[[m]])))
                stop("Invalid update object for ", m)
            default[[m]] <- update[[m]]
        } else if (length(inherit) && m %in% names(inherit)) {
            if (!identical(class(default[[m]]), class(inherit[[m]])))
                stop("Invalid inherit object for ", m)
            default[[m]] <- inherit[[m]]
        }
    }
    return(default)
}

# field_system -----------

#' Shortcut functions to access or assign metadata
#'
#' Internal functions to access or replace an object metadata field without
#' going through attribute trees. `field_system()`, `field_object()` and
#' `field_user()` correspond to the system, object and user meta fields,
#' respectively.
#' @rdname field_system
#' @param x a list of attributes extracted from a `dfm`, `tokens`, or `corpus`
#'   object by `attributes(x)`
#' @param field name of the sub-field to access or assign values
#' @keywords internal
field_system <- function(x, field = NULL) {
    if (is.null(field)) {
        return(x$meta$system)
    }
    return(x$meta$system[[field]])
}

#' @rdname field_system
`field_system<-` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x$meta$system <- value
    } else {
        x$meta$system[[field]] <- value
    }
    return(x)
}

#' @rdname field_system
field_object <- function(x, field = NULL) {
    if (is.null(field)) {
        return(x$meta$object)
    }
    return(x$meta$object[[field]])
}

#' @rdname field_system
`field_object<-` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x$meta$object <- value
    } else {
        x$meta$object[[field]] <- value
    }
    return(x)
}

#' @rdname field_system
field_user <- function(x, field = NULL) {
    if (is.null(field)) {
        return(x$meta$user)
    }
    return(x$meta$user[[field]])
}

#' @rdname field_system
`field_user<-` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        x$meta$user <- value
    } else {
        x$meta$user[[field]] <- value
    }
    return(x)
}
