# metacorpus ---------------------

#' Get or set corpus metadata
#' 
#' Get or set the corpus-level metadata in a \link{corpus} object.
#' 
#' \code{metacorpus} and \code{metacorpus<-} are synonyms but are deprecated.
#' @param x a \link{corpus} object
#' @param field metadata field name(s);  if \code{NULL} (default), return all 
#'   metadata names
#' @param type \code{"user"} for user-provided corpus-level meta-data; \code{"system"} for 
#' meta-data set automatically when the corpus is created; or \code{"all"} for all meta-data.
#' @return For \code{meta}, a named list of the metadata fields in the corpus.
#'
#'   For \code{meta <-}, the corpus with the updated user-level metadata.  Only
#'   user-level meta-data may be assigned.
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
    type <- match.arg(type)
    result <- list()
    if (type %in% c("user", "all"))
        result <- c(result, attr(x, "meta")$user)
    if (type %in% c("system", "all"))
        result <- c(result, attr(x, "meta")$system)
    return(if (is.null(field)) result else result[field])
}

#' @export
meta.tokens <- meta.corpus


# meta<-   -----------

#' Replacement function for corpus-level data
#' @param value new value of the corpus metadata field
#' @export
#' @rdname meta
#' @aliases "metacorpus<-"
"meta<-" <- "metacorpus<-" <- function(x, field = NULL, value) {
    if (is.null(field) && !is.list(value)) stop("value must be a named list")
    if (length(names(value)) > 0 && length(names(value)) != length(value))
        stop("every element of value must be named")
    UseMethod("meta<-")
}

#' @export
"meta<-.default" <- function(x, field = NULL, value) {
    stop(friendly_class_undefined_message(class(x), "meta<-"))
}

#' @export
`meta<-.corpus` <- function(x, field = NULL, value) {
    if (is.null(field)) {
        attr(x, "meta")$user <- value
    } else {
        attr(x, "meta")$user[field] <- value
    }
    return(x)
}

#' @export
`meta<-.tokens` <- `meta<-.corpus`

# legacy functions ----------

#' @rdname meta
#' @export
metacorpus <- meta

#' @rdname meta
#' @export
`metacorpus<-` <- `meta<-`



# internal: meta_initialize ----------------

#' Internal function to initialize meta-data
#' 
#' Initializes meta-data for new objects.  The "system" element of this list is
#' for automatically set system data; the "user" element of this list is set by
#' the user and replaces the \link{metacorpus} function.
#' @param x input object for which the metadata will be set
#' @param source character; the input object class
#' @param list; key-value set of user metadata
#' @keywords internal corpus
meta_init <- function(x, source, user = list()) {
    # type checking
    stopifnot(is.character(source))
    stopifnot(is.list(user))
    # make sure every element of user is named
    if (length(user)) stopifnot(length(names(user)) == length(user))
    UseMethod("meta_init")
}
    
#' @rdname meta_init
meta_init.corpus <- function(x, source, user = list()) {
    # set system meta, initialize user meta
    attr(x, "meta") <- 
        list(user = list(),
             system = list(
                 "source" = source,
                 "package-version" = utils::packageVersion("quanteda"),
                 "r-version" = getRversion(),
                 "system" = Sys.info()[c("sysname", "machine", "user")],
                 "directory" = getwd(),
                 "created" = Sys.Date()
             )
        )
    # set the user meta
    meta(x) <- user
    return(x)
}

#' @rdname meta_init
meta_init.tokens <- meta_init.corpus
