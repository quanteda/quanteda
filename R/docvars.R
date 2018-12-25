
# internal function to modify docvars while protecting system-level variables
"set_docvars<-" <- function(x, field = NULL, value) {
    stopifnot(is.data.frame(x))
    flag <- is_system(names(x))
    if (is.dfm(value))
        value <- convert(value, to = "data.frame")[-1]
    if (is.null(value)) {
        x <- x[flag]
    } else if (is.null(field) && (is.data.frame(value))) {
        if (nrow(value) != nrow(x))
            stop(message_error("docvar_mismatch"))
        x <- cbind(x[flag], value)
    } else {
        if (any(is_system(field)))
            stop(message_error("docvar_invalid"))
        x[field] <- value
    }
    rownames(x) <- NULL
    return(x)
}

#' Internal function to extract docvars
#' @param x an object from which docvars are extracted
#' @param field name of docvar fields
#' @param system if \code{TRUE}, return system-level variables
#' @param drop if \code{TRUE}, convert data.frame with one variable to a vector
#' @keywords internal
get_docvars <- function(x, field = NULL, system = FALSE, drop = FALSE) {
    UseMethod("get_docvars")
}

#' @method get_docvars corpus
get_docvars.corpus <- function(x, field = NULL, system = FALSE, drop = FALSE) {
    get_docvars(attr(x, "docvars"), field, system, drop)
}

#' @method get_docvars tokens
get_docvars.tokens <- function(x, field = NULL, system = FALSE, drop = FALSE) {
    get_docvars(attr(x, "docvars"), field, system, drop)
}

#' @method get_docvars dfm
get_docvars.dfm <- function(x, field = NULL, system = FALSE, drop = FALSE) {
    get_docvars(x@docvars, field, system, drop)
}

#' @method get_docvars data.frame
get_docvars.data.frame <- function(x, field = NULL, system = FALSE, drop = FALSE) {
    x <- x[system == is_system(names(x))]
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
        data.frame("_docid" = character(),
                   "_docname" = factor(),
                   "_docnum" = integer(), 
                   "_segnum" = integer(), 
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    } else {
        if (unique) {
            docnum <- match(docname, unique(docname))
            if (any(duplicated(docname))) {
                segnum <- ave(docname == docname, docname, FUN = cumsum)
                docid <- paste0(docname, ".", segnum)
            } else {
                segnum <- rep(1L, n)
                docid <- docname
            }
        } else {
            docnum <- seq(1L, n)
            segnum <- rep(1L, n)
            docid <- docname
        }
        data.frame("_docid" = docid,
                   "_docname" = factor(docname, levels = unique(docname)),
                   "_docnum" = docnum, 
                   "_segnum" = segnum,
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    }
}

# internal function to upgrade docvars to modern format
upgrade_docvars <- function(x, docname = NULL) {
    if (sum(is_system(colnames(x))) == 4) 
        return(x)
    if (is.null(docname)) 
        docname <- rownames(x)
    if (is.null(x) || length(x) == 0) {
        result <- make_docvars(length(docname), docname, FALSE)
    } else {
        rownames(x) <- NULL
        result <- cbind(make_docvars(nrow(x), docname, FALSE), 
                        x[!is_system(names(x)) & !!is_system_old(names(x))])
        if ("_document" %in% names(x))
            result[["_docname"]] <- factor(x[["_document"]], levels = unique(x[["_document"]]))
        if ("_docid" %in% names(x))
            result[["_docnum"]] <- as.integer(x[["_docid"]])
        if ("_segid" %in% names(x))
            result[["_segnum"]] <- as.integer(x[["_segid"]])
    }
    return(result)
}

# internal function to check if variables are internal-only
is_system <- function(x) {
    x %in% c("_docid", "_docname", "_docnum", "_segnum")
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
    get_docvars(attr(x, 'docvars'), field, FALSE, TRUE)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    get_docvars(attr(x, 'docvars'), field, FALSE, TRUE)
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    x <- as.dfm(x)
    get_docvars(x@docvars, field, FALSE, TRUE)
}

#' @noRd
#' @keywords internal
docvars.kwic <- function(x) {
    get_docvars(attr(x, 'docvars'), NULL)
}

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
#' @section Index access to docvars in a corpus:
#' Another way to access and set docvars is through indexing of the corpus 
#' \code{j} element, such as \code{data_corpus_irishbudget2010[, c("foren", 
#' "name"]}; or, for a single docvar, 
#' \code{data_corpus_irishbudget2010[["name"]]}.  The latter also permits 
#' assignment, including the easy creation of new document variables, e.g. 
#' \code{data_corpus_irishbudget2010[["newvar"]] <- 
#' 1:ndoc(data_corpus_irishbudget2010)}. See \code{\link{[.corpus}} for details.
#' 
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' # assigning document variables to a corpus
#' corp <- data_corpus_inaugural
#' docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' 
#' # alternative using indexing
#' head(corp[, "Year"])
#' corp[["President2"]] <- paste("prezTwo", 1:ndoc(corp), sep = "")
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
