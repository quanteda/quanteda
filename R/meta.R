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
meta <- function(x, field = NULL, type = c("user", "system", "all"))
    UseMethod("meta")

#' @export
meta.default <- function(x, field = NULL, type = c("user", "system", "all")) {
    stop(friendly_class_undefined_message(class(x), "meta"))
}

#' @export
meta.corpus <- function(x, field = NULL, type = c("user", "system", "all")) {
    if (is_pre2(x)) return(if (is.corpus(x)) x$metadata else NULL)
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, attr(x, "meta")$user)
    if (type %in% c("system", "all"))
        result <- c(result, attr(x, "meta")$system)
    if (is.null(field)) {
        return(result)
    } else {
        return(result[[field]])
    }
}

#' @export
meta.tokens <- meta.corpus

#' @export
meta.dfm <- function(x, field = NULL, type = c("user", "system", "all")) {
    if (is_pre2(x)) return(NULL)
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, x@meta$user)
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
        attr(x, "meta")$system[field] <- value
    }
    return(x)
}

#' @rdname meta_system
`meta_system<-.tokens` <- function(x, field = NULL, value) {
    if (is.null(field) && !missing(value)) {
        attr(x, "meta")$system <- value
    } else {
        attr(x, "meta")$system[field] <- value
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
meta_system_defaults <- function(source) {
    list("source" = source,
         "package-version" = utils::packageVersion("quanteda"),
         "r-version" = getRversion(),
         "system" = Sys.info()[c("sysname", "machine", "user")],
         "directory" = getwd(),
         "created" = Sys.Date()
    )
}
