# metacorpus ---------------------

#' Get or set corpus metadata
#'
#' Get or set the corpus-level metadata in a [corpus] object.
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
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, attr(x, "meta")$user)
    if (type %in% c("object", "all"))
        result <- c(result, attr(x, "meta")$object)
    if (type %in% c("system", "all"))
        result <- c(result, attr(x, "meta")$system)
    if (is.null(field)) {
        return(result)
    } else {
        return(result[[field]])
    }
}

#' @export
meta.tokens <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x)) 
        return(NULL)
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, attr(x, "meta")$user)
    if (type %in% c("object", "all"))
        result <- c(result, attr(x, "meta")$object)
    if (type %in% c("system", "all"))
        result <- c(result, attr(x, "meta")$system)
    if (is.null(field)) {
        return(result)
    } else {
        return(result[[field]])
    }
}

#' @export
meta.dfm <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    if (is_pre2(x)) 
        return(NULL)
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, x@meta$user)
    if (type %in% c("object", "all"))
        result <- c(result, x@meta$object)
    if (type %in% c("system", "all"))
        result <- c(result, x@meta$system)
    if (is.null(field)) {
        return(result)
    } else {
        return(result[[field]])
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

# #' @export
# "meta<-.default" <- function(x, field = NULL, value) {
#     stop(friendly_class_undefined_message(class(x), "meta<-"))
# }

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

make_meta_system <- meta_system_defaults # for development

make_meta <- function(class, inherit = NULL, ...) {
    
    result <- list(
        "system" = make_meta_system(),
        "object" = list(),
        "user" = list()
    )
    if ("system" %in% names(inherit))
        result$system <- inherit$system
    if (class == "corpus") {
        result$object <- make_meta_corpus(inherit$object, ...)
    } else if (class == "tokens") {
        result$object <- make_meta_tokens(inherit$object, ...)
    } else if (class == "dfm") {
        result$object <- make_meta_dfm(inherit$object, ...)
    }
    if ("user" %in% names(inherit))
        result$user <- inherit$user
    # comming soon...
    # else if (class == "dictionary2") {
    #   make_meta_dictionary2(inherit, ...)
    #}
    return(result)
}

make_meta_corpus <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list("unit" = "documents")
    update_meta(default, inherit, ...)
}

make_meta_tokens <- function(inherit = NULL, ...) {
    if (is.null(inherit))
        inherit <- list()
    default <- list(
        "unit" = "documents",
        "what" = "word", 
        "ngram" = 1L, 
        "skip" = 0L,
        "concatenator" = "_"
    )
    update_meta(default, inherit, ...)
}

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
        "smooth" = 0
    )
    update_meta(default, inherit, ...)
}

get_meta <- function(x) {
    if (isS4(x)) {
        return(x@meta)
    } else {
        return(attr(x, "meta"))
    }
}

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

#' Internal functions to access meta filed in a list of attributes
#' @rdname field_system
#' @inheritParams meta_system
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
