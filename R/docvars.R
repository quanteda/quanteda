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
    get_docvars(attr(x, 'docvars'), field)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    get_docvars(attr(x, 'docvars'), field)
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    x <- as.dfm(x)
    get_docvars(x@docvars, field)
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
    attr(x, "docvars") <- set_docvars(attr(x, "docvars"), field, value)
    return(x)
}

#' @export
"docvars<-.tokens" <- function(x, field = NULL, value) {
    attr(x, "docvars") <- set_docvars(attr(x, "docvars"), field, value)
    return(x)
}

#' @export
"docvars<-.dfm" <- function(x, field = NULL, value) {
    x <- as.dfm(x)
    x@docvars <- set_docvars(x@docvars, field, value)
    return(x)
}

# internal function to modify docvars
set_docvars <- function(x, field, value) {
    
    flag <- is_internal(names(x))
    if (is.dfm(value))
        value <- convert(value, to = "data.frame")[-1]
    if (is.null(value)) {
        x[!flag] <- NULL
    } else if (is.null(field) && (is.data.frame(value))) {
        if (nrow(value) != ndoc(x))
            stop(message_error("docvar_mismatch"))
        x <- cbind(x[flag], value)
    } else if (!any(is_internal(field))) {
        x[[field]] <- value
    } else {
        message_error("docvar_invalid")
    }
    return(x)
}

## internal function to return the docvars for all docvars functions
get_docvars <- function(x, field = NULL) {
    x <- x[!is_internal(names(x))]
    if (is.null(field)) {
        return(x)
    } else {
        return(x[field])
    }
}
