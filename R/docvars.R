
# internal function to modify user docvars only
"set_docvars<-" <- function(x, field, system = FALSE, value) {
    stopifnot(is.data.frame(x))
    flag <- system == is_system(names(x))
    if (is.dfm(value))
        value <- convert(value, to = "data.frame")[-1]
    if (is.null(value)) {
        x <- x[!flag]
    } else if (is.null(field) && (is.data.frame(value))) {
        if (nrow(value) != nrow(x))
            stop(message_error("docvar_mismatch"))
        x <- cbind(x[flag], value)
    } else {
        if (any(is_system(field)))
            stop(message_error("docvar_invalid"))
        if (system) 
            field <- paste0("_", field)
        x[field] <- value
    }
    rownames(x) <- NULL
    return(x)
}

#' Internal function to extract docvars
#' @param x an object from which docvars are extracted
#' @param field name of docvar fields
#' @param system if \code{TRUE}, treat field as system-level variable
#' @param drop if \code{TRUE}, covert data.frame with one variable to a vector
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
    stopifnot(is.data.frame(x))
    x <- x[system == is_system(names(x))]
    if (is.null(field)) {
        return(x)
    } else {
        if (system) 
            x <- x[paste0("_", field)]
        if (length(x) == 1 && drop) {
            return(x[[1]])
        } else {
            return(x)
        }
    }
}

# internal function to make new system-level docvars
make_docvars <- function(docname, unique = TRUE) {
    docname <- as.character(docname)
    if (unique)
        docname <- make.unique(docname)
    n <- length(docname)
    if (n == 0) {
        data.frame("_docid" = character(),
                   "_docname" = character(),
                   "_docnum" = integer(), 
                   "_segnum" = integer(), 
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    } else {
        data.frame("_docid" = docname,
                   "_docname" = docname,
                   "_docnum" = seq(1L, n), 
                   "_segnum" = rep(1L, n), 
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    }
}

# internal function to upgrade docvars to modern format
upgrade_docvars <- function(x, docname = NULL) {
    if (is.null(docname))
        docname <- rownames(x)
    rownames(x) <- NULL
    if (sum(is_system(colnames(x))) == 4) {
        return(x)
    } else if (is.null(x) || length(x) == 0) {
        return(make_docvars(docname, FALSE))
    } else {
        return(cbind(make_docvars(docname, FALSE), get_docvars(x)))
    }
}

# internal function to check if variable is internal-only
is_system <- function(x) {
    x %in% c("_docid", "_docname", "_docnum", "_segnum")
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
    get_docvars(attr(x, 'docvars'), field, drop = TRUE)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    get_docvars(attr(x, 'docvars'), field, drop = TRUE)
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    x <- as.dfm(x)
    get_docvars(x@docvars, field, drop = TRUE)
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
    set_docvars(attr(x, "docvars"), field, FALSE) <- value
    return(x)
}

#' @export
"docvars<-.tokens" <- function(x, field = NULL, value) {
    set_docvars(attr(x, "docvars"), field, FALSE) <- value
    return(x)
}

#' @export
"docvars<-.dfm" <- function(x, field = NULL, value) {
    x <- as.dfm(x)
    set_docvars(x@docvars, field, FALSE) <- value
    return(x)
}
