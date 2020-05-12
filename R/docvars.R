# internal methods -------

# internal function to modify docvars while protecting system-level variables
"set_docvars<-" <- function(x, field = NULL, value) {
    stopifnot(is.data.frame(x))
    flag <- is_system(names(x))
    if (is.dfm(value))
        value <- convert(value, to = "data.frame")[-1]
    if (is.null(field)) {
        if (is.null(value)) {
            x <- x[flag]
        } else {
            if (is.matrix(value))
                value <- as.data.frame(value, stringsAsFactors = FALSE)
            if (is.data.frame(value)) {
                if (nrow(value) != nrow(x))
                    stop(message_error("docvar_mismatch"))
                if (!is.character(names(value)) || length(names(value)) != ncol(value) ||
                    any(is.na(names(value)))) {
                    stop(message_error("docvar_nocolname"), call. = FALSE)
                }
                x <- cbind(x[flag], value)
            } else {
                stop(message_error("docvar_nofield"), call. = FALSE)
            }
        }
    } else {
        if (any(is_system(field)))
            stop(message_error("docvars_invalid"))
        x[field] <- value
    }
    rownames(x) <- NULL
    return(x)
}

#' Internal function to extract docvars
#' @param x an object from which docvars are extracted
#' @param field name of docvar fields
#' @param user if `TRUE`, return user variables
#' @param system if `TRUE`, return system variables
#' @param drop if `TRUE`, convert data.frame with one variable to a vector
#' @keywords internal
get_docvars <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    UseMethod("get_docvars")
}

#' @method get_docvars corpus
get_docvars.corpus <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    select_docvars(attr(x, "docvars"), field, user, system, drop)
}

#' @method get_docvars tokens
get_docvars.tokens <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    select_docvars(attr(x, "docvars"), field, user, system, drop)
}

#' @method get_docvars dfm
get_docvars.dfm <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    select_docvars(x@docvars, field, user, system, drop)
}

# Internal function to select columns of docvars
select_docvars <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    x <- x[user * !is_system(names(x)) | system * is_system(names(x))]
    if (is.null(field)) {
        return(x)
    } else {
        error <- !field %in% names(x)
        if (any(error))
            stop("field(s) ", paste(field[error], collapse = ", "), " not found")
        if (length(field) == 1 && drop) {
            return(x[[field]])
        } else {
            return(x[field])
        }
    }
}

# internal function to make new system-level docvars
make_docvars <- function(n, docname = NULL, unique = TRUE) {
    
    stopifnot(is.integer(n))
    if (is.null(docname)) {
        docname <- paste0(quanteda_options("base_docname"), seq_len(n))
    } else {
        stopifnot(n == length(docname))
        docname <- stri_trans_nfc(as.character(docname))
    }
    if (n == 0) {
        result <- data.frame("docname_" = character(),
                             "docid_" = factor(),
                             "segid_" = integer(),
                              stringsAsFactors = FALSE)
    } else {
        if (unique) {
            if (any(duplicated(docname))) {
                segid <- stats::ave(docname == docname, docname, FUN = cumsum)
                docid <- paste0(docname, ".", segid)
            } else {
                segid <- rep(1L, n)
                docid <- docname
            }
        } else {
            segid <- rep(1L, n)
            docid <- docname
        }
        result <- data.frame("docname_" = docid,
                             "docid_" = factor(docname, levels = unique(docname)),
                             "segid_" = segid,
                             stringsAsFactors = FALSE)
    }
    rownames(result) <- NULL
    return(result)
}

#' Internal function to subset or duplicate docvar rows
#' @param x docvar data.frame
#' @param i numeric or logical vector for subsetting/duplicating rows
#' @keywords internal
reshape_docvars <- function(x, i = NULL) {
    if (is.null(i)) return(x)
    x <- x[i, , drop = FALSE]
    temp <- make_docvars(nrow(x), x[["docid_"]], TRUE)
    x[c("docname_", "docid_", "segid_")] <- temp
    rownames(x) <- NULL
    return(x)
}

# Reshape docvars keeping variables that have the same values within groups
group_docvars <- function(x, group = NULL) {
    if (is.null(group))
        return(x)
    l <- rep(FALSE, length(x))
    for (i in seq_along(l)) {
        if (is_system(names(x)[i]) || is_grouped(x[[i]], group)) {
            l[i] <- TRUE
        }
    }
    temp <- make_docvars(length(levels(group)), levels(group), TRUE)
    result <- x[match(levels(group), group), l, drop = FALSE]
    result[c("docname_", "docid_", "segid_")] <- temp
    rownames(result) <- NULL
    return(result)
}


# internal function to upgrade docvars to modern format
upgrade_docvars <- function(x, docname = NULL) {
    if (sum(is_system(colnames(x))) == 3)
        return(x)
    if (is.null(docname))
        docname <- rownames(x)
    if (is.null(x) || length(x) == 0) {
        result <- make_docvars(length(docname), docname, FALSE)
    } else {
        result <- cbind(make_docvars(nrow(x), docname, FALSE),
                        x[!is_system(names(x)) & !is_system_old(names(x))])
        if ("_document" %in% names(x))
            result[["docid_"]] <- factor(x[["_document"]], levels = unique(x[["_document"]]))
        if ("_segid" %in% names(x))
            result[["segid_"]] <- as.integer(x[["_segid"]])
    }
    rownames(result) <- NULL
    return(result)
}

# internal function to check if variables are internal-only
is_system <- function(x) {
    x %in% c("docname_", "docid_", "segid_")
}

# internal function to check if old variables are internal-only
is_system_old <- function(x) {
    x %in% c("texts", "_document", "_docid", "_segid")
}

# core docvars methods -------

#' Get or set document-level variables
#'
#' Get or set variables associated with a document in a [corpus],
#' [tokens] or [dfm] object.
#' @param x [corpus], [tokens], or [dfm] object whose
#'   document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return `docvars` returns a data.frame of the document-level variables,
#'   dropping the second dimension to form a vector if a single docvar is
#'   returned.
#' @section Accessing or assigning docvars using the `$` operator:
#' As of \pkg{quanteda} v2, it is possible to access and assign a docvar using
#' the `$` operator.  See Examples.
#' @examples
#' # retrieving docvars from a corpus
#' head(docvars(data_corpus_inaugural))
#' tail(docvars(data_corpus_inaugural, "President"), 10)
#' head(data_corpus_inaugural$President)
#'
#' @export
#' @keywords corpus
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

#' @export
docvars.default <- function(x, field = NULL) {
    stop(friendly_class_undefined_message(class(x), "docvars"))
}

#' @noRd
#' @export
    docvars.corpus <- function(x, field = NULL) {
    x <- as.corpus(x)
    select_docvars(attr(x, "docvars"), field, user = TRUE, system = FALSE, drop = TRUE)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    x <- as.tokens(x)
    select_docvars(attr(x, "docvars"), field, user = TRUE, system = FALSE, drop = TRUE)
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    x <- as.dfm(x)
    select_docvars(x@docvars, field, user = TRUE, system = FALSE, drop = TRUE)
}

#' @noRd
#' @keywords internal
docvars.kwic <- function(x) {
    select_docvars(attr(x, "docvars"), NULL)
}

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Reassigning document variables for a [tokens] or [dfm] object
#' is allowed, but discouraged.  A better, more reproducible workflow is to
#' create your docvars as desired in the [corpus], and let these continue
#' to be attached "downstream" after tokenization and forming a document-feature
#' matrix.  Recognizing that in some cases, you may need to modify or add
#' document variables to downstream objects, the assignment operator is defined
#' for [tokens] or [dfm] objects as well.  Use with caution.
#'
#' @return `docvars<-` assigns `value` to the named `field`
#' @examples
#' # assigning document variables to a corpus
#' corp <- data_corpus_inaugural
#' docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' corp$fullname <- paste(data_corpus_inaugural$FirstName,
#'                        data_corpus_inaugural$President)
#' tail(corp$fullname)
#'
#' @export
"docvars<-" <- function(x, field = NULL, value) {
    UseMethod("docvars<-")
}

#' @export
"docvars<-.default" <- function(x, field = NULL, value) {
    stop(friendly_class_undefined_message(class(x), "docvars<-"))
}

#' @export
"docvars<-.corpus" <- function(x, field = NULL, value) {
    x <- as.corpus(x)
    set_docvars(attr(x, "docvars"), field) <- value
    return(x)
}

#' @export
"docvars<-.tokens" <- function(x, field = NULL, value) {
    set_docvars(attr(x, "docvars"), field) <- value
    return(x)
}

#' @export
"docvars<-.dfm" <- function(x, field = NULL, value) {
    x <- as.dfm(x)
    set_docvars(x@docvars, field) <- value
    return(x)
}

# $ methods -----------

access_dvs <- function(x, name) {
    if (!name %in% names(docvars(x)))
        return(NULL)
    else
        return(docvars(x, name))
}

assign_dvs <- function(x, name, value) {
    docvars(x, name) <- value
    return(x)
}

#' @rdname docvars
#' @method $ corpus
#' @param name a literal character string specifying a single [docvars] name
#' @export
#' @examples
#' 
#' # accessing or assigning docvars for a corpus using "$"
#' data_corpus_inaugural$Year
"$.corpus" <- access_dvs

#' @rdname docvars
#' @method $<- corpus
#' @param value a vector of document variable values to be assigned to `name`
#' @export
#' @examples
#' data_corpus_inaugural$century <- floor(data_corpus_inaugural$Year / 100)
#' data_corpus_inaugural$century
"$<-.corpus" <- assign_dvs

#' @rdname docvars
#' @method $ tokens
#' @export
#' @examples
#'
#' # accessing or assigning docvars for tokens using "$"
#' toks <- tokens(corpus_subset(data_corpus_inaugural, Year <= 1805))
#' toks$Year
"$.tokens" <- access_dvs

#' @rdname docvars
#' @method $<- tokens
#' @export
#' @examples
#' toks$Year <- 1991:1995
#' toks$Year
#' toks$nonexistent <- TRUE
#' docvars(toks)
"$<-.tokens" <- assign_dvs

#' @rdname docvars
#' @method $ dfm
#' @export
#' @examples
#'
#' # accessing or assigning docvars for a dfm using "$"
#' dfmat <- dfm(toks)
#' dfmat$Year
"$.dfm" <- access_dvs

#' @rdname docvars
#' @method $<- dfm
#' @export
#' @examples
#' dfmat$Year <- 1991:1995
#' dfmat$Year
#' dfmat$nonexistent <- TRUE
#' docvars(dfmat)
"$<-.dfm" <- assign_dvs

#' @noRd
#' @method $ fcm
#' @export
"$.fcm" <- function(x, name) {
    stop("$ not defined for an fcm object", call. = FALSE)
}

#' @noRd
#' @method $<- fcm
#' @export
"$<-.fcm" <- function(x, name, value) {
    stop("$<- not defined for an fcm object", call. = FALSE)
}
