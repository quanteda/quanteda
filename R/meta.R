# meta ---------------------

#' Get or set object metadata
#'
#' Get or set metadata in a [corpus], [tokens], [dfm], or [dictionary] object.
#'
#' `metacorpus` and `metacorpus<-` are synonyms for `meta()` and `meta()<-` for
#' [corpus] objects, but are deprecated.
#' @param x an object for which the metadata will be read or set
#' @param field metadata field name(s); if `NULL` (default), return all
#'   metadata names
#' @param type `"user"` for user-supplied metadata;
#'   `"system"` for metadata set automatically when the object is created;
#'   or `"all"` for all metadata.
#' @return For `meta`, a named list of the metadata fields.
#'
#'   For `meta <-`, the object with the updated user-level metadata.  Only
#'   user-level metadata may be assigned.
#' @export
#' @keywords meta
#' @aliases metacorpus
#' @examples
#' meta(data_corpus_inaugural)
#' meta(data_corpus_inaugural, "source")
#' meta(data_corpus_inaugural, "citation") <- "Presidential Speeches Online Project (2014)."
#' meta(data_corpus_inaugural, "citation")
#'
#' dict <- dictionary(list(neg = c("bad", "awful"), pos = "good"))
#' meta(dict, "source") <- "The example from meta()."
#' meta(dict)
meta <- function(x, field = NULL, type = c("user", "system", "all")) {
    check_valid_meta_object(x)

    if (is_pre2(x)) {
        if (is.corpus(x)) {
            return(if (is.corpus(x)) x[["metadata"]] else NULL)
        } else if (is.dfm(x) | is.tokens(x)) {
            return(NULL)
        }
    }

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


# meta<-   -----------

#' Replacement function for corpus-level data
#' @param value new value of the metadata field
#' @export
#' @rdname meta
#' @aliases "metacorpus<-"
"meta<-" <- "metacorpus<-" <- function(x, field = NULL, value) {
    check_valid_meta_object(x)

    if (is.null(field) && !is.list(value)) stop("value must be a named list")
    if (is.list(value) && length(names(value)) != length(value))
        stop("every element of the meta list must be named")

    if (is.null(field)) {
        attr(x, "meta")$user <- value
    } else {
        attr(x, "meta")$user[[field]] <- value
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

# internal functions ----------------

#' Check if an object is eligible for meta functions
#'
#' Check the class of an object to see if it is eligible for `meta()`-type
#' functions.` This avoids having to make the meta functions object-oriented.
#' Even though [dfm] and [dictionary] functions are S4, the same code works
#' because the [attr()] calls work for named slots (since they are just special
#' attributes).
#' @param x an object to be checked for eligibility
#' @return nothing; halts with an error if `x` is ineligible
#' @importFrom stringi stri_replace_all_regex
#' @keywords internal meta
check_valid_meta_object <- function(x) {
    if (!(is.corpus(x) | is.tokens(x) | is.dictionary(x) | is.dfm(x))) {
        parent_fun <- deparse(sys.calls()[[sys.nframe() - 1]])
        stop("\b\b in ", parent_fun, " :\n  ",
             stri_replace_all_regex(parent_fun, "\\(.*\\)$", ""),
             "() only works on corpus, tokens, dfm, or dictionary objects",
             call. = FALSE)
    }
}

#' Internal function to get, set or initialize system metadata
#'
#' Sets or initializes system metadata for new objects.
#' @inheritParams meta
#' @return `meta_system` returns a list with the object's system metadata.
#'   It is literally a wrapper to [`meta(x, field, type =
#'   "system")()`][meta].
#' @keywords internal meta
#' @examples
#' corp <- corpus(c(d1 = "one two three", d2 = "two three four"))
#' # quanteda:::`meta_system<-`(corp, value = quanteda:::meta_system_defaults("example"))
#' quanteda:::meta_system(corp)
meta_system <- function(x, field = NULL) {
    check_valid_meta_object(x)
    meta(x, field = field, type = "system")
}

#' @rdname meta_system
#' @return `meta_system<-` returns the object with the system metadata
#'   modified. This is an internal function and not designed for users!
`meta_system<-` <- function(x, field = NULL, value) {
    check_valid_meta_object(x)

    if (is.null(field) && !is.list(value)) stop("value must be a named list")
    if (is.list(value) && length(names(value)) != length(value))
        stop("every element of the meta list must be named")
    if (is.null(field) && !missing(value)) {
        attr(x, "meta")$system <- value
    } else {
        attr(x, "meta")$system[[field]] <- value
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
