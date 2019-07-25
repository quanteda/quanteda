
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
#' @param user if \code{TRUE}, return user variables
#' @param system if \code{TRUE}, return system variables
#' @param drop if \code{TRUE}, convert data.frame with one variable to a vector
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
        docname <- as.character(docname)
    }
    if (n == 0) {
        result <- data.frame("docname_" = character(),
                             "docid_" = factor(),
                             "segid_" = integer(), 
                              stringsAsFactors = FALSE)
    } else {
        if (unique) {
            docnum <- match(docname, unique(docname))
            if (any(duplicated(docname))) {
                segid <- stats::ave(docname == docname, docname, FUN = cumsum)
                docid <- paste0(docname, ".", segid)
            } else {
                segid <- rep(1L, n)
                docid <- docname
            }
        } else {
            docnum <- seq(1L, n)
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

# internal function to duplicate or dedplicate docvar rows
reshape_docvars <- function(x, i = NULL) {
    if (is.null(i)) return(x)
    x <- x[i,, drop = FALSE]
    if (is.numeric(i) && any(duplicated(i))) {
        x[["segid_"]] <- stats::ave(i == i, i, FUN = cumsum)
        x[["docname_"]] <- paste0(x[["docid_"]], ".", x[["segid_"]])
    } else {
        x[["segid_"]] <- rep(1L, nrow(x))
        x[["docname_"]] <- as.character(x[["docid_"]])
    }
    rownames(x) <- NULL
    return(x)
}

subset_docvars <- function(x, i = NULL) {
    if (is.null(i)) return(x)
    x <- x[i,, drop = FALSE]
    rownames(x) <- NULL
    return(x)
}

# Reshape docvars keeping variables that have the same values within groups
group_docvars <- function(x, group) {
    l <- is_system(names(x)) | unlist(lapply(x, is_grouped, group), use.names = FALSE)
    result <- x[match(levels(group), group), l, drop = FALSE]
    result[["docname_"]] <- levels(group)
    rownames(result) <- NULL
    return(result)
}


# internal function to upgrade docvars to modern format
upgrade_docvars <- function(x, docnames = NULL) {
    if (sum(is_system(colnames(x))) == 3) 
        return(x)
    if (is.null(docnames))
        docnames <- rownames(x)
    if (is.null(x) || length(x) == 0) {
        result <- make_docvars(length(docnames), docnames, FALSE)
    } else {
        result <- cbind(make_docvars(nrow(x), docnames, FALSE), 
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


#' Get or set document-level variables
#' 
#' Get or set variables associated with a document in a \link{corpus},
#' \link{tokens} or \link{dfm} object.
#' @param x \link{corpus}, \link{tokens}, or \link{dfm} object whose 
#'   document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables, 
#'   dropping the second dimension to form a vector if a single docvar is 
#'   returned.
#' @examples 
#' # retrieving docvars from a corpus
#' head(docvars(data_corpus_inaugural))
#' tail(docvars(data_corpus_inaugural, "President"), 10)
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
    select_docvars(attr(x, 'docvars'), field, user = TRUE, system = FALSE, drop = TRUE)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    x <- as.tokens(x)
    select_docvars(attr(x, 'docvars'), field, user = TRUE, system = FALSE, drop = TRUE)
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
    select_docvars(attr(x, 'docvars'), NULL)
}

# #' @noRd
# #' @export
# `$.corpus` <- function(x, value) {
#     x <- as.corpus(x)
#     select_docvars(attr(x, 'docvars'), value, user = TRUE, system = FALSE, drop = TRUE)
# }
# 
# #' @noRd
# #' @export
# `$.tokens` <- function(x, value) {
#     x <- as.tokens(x)
#     select_docvars(attr(x, 'docvars'), value, user = TRUE, system = FALSE, drop = TRUE)
# }
# 
# #' @noRd
# #' @export
# `$.dfm` <- function(x, value) {
#     x <- as.dfm(x)
#     select_docvars(x@docvars, value, user = TRUE, system = FALSE, drop = TRUE)
# }

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Reassigning document variables for a \link{tokens} or \link{dfm} object
#' is allowed, but discouraged.  A better, more reproducible workflow is to
#' create your docvars as desired in the \link{corpus}, and let these continue
#' to be attached "downstream" after tokenization and forming a document-feature
#' matrix.  Recognizing that in some cases, you may need to modify or add
#' document variables to downstream objects, the assignment operator is defined
#' for \link{tokens} or \link{dfm} objects as well.  Use with caution.
#' 
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' # assigning document variables to a corpus
#' corp <- data_corpus_inaugural
#' docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
#' head(docvars(corp)) 
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
