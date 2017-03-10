#' get or set for document-level variables
#' 
#' Get or set variables associated with a document in a \link{corpus}, or get
#' these variables from a \link{tokens} or \link{dfm} object.
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

#' @noRd
#' @export
docvars.corpus <- function(x, field = NULL) {
    check_fields(x, field)
    dvars <- documents(x)[, which(names(documents(x)) != "texts"), drop = FALSE]
    if (is.null(field))
        dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    check_fields(x, field)
    dvars <- attr(x, "docvars")[, , drop = FALSE]
    if (is.null(field))
        dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
    get_docvars(dvars, field)    
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    check_fields(x, field)
    dvars <- x@docvars[, , drop = FALSE]
    if (is.null(field))
        dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
    get_docvars(dvars, field)    
}

## internal function to return the docvars for all docvars functions
get_docvars <- function(dvars, field = NULL) {
    if (is.null(field)) {
        if (length(dvars) == 0)
            return(data.frame())
        else
            return(dvars)
    } else {
        return(dvars[, field, drop = TRUE])
    }
}


#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Another way to access and set docvars is through indexing of the corpus
#'   \code{j} element, such as \code{data_corpus_irishbudget2010[, c("foren", 
#'   "name"]} or for a single docvar, 
#'   \code{data_corpus_irishbudget2010[["name"]]}.  The latter also permits 
#'   assignment, including the easy creation of new document varibles, e.g. 
#'   \code{data_corpus_irishbudget2010[["newvar"]] <- 
#'   1:ndoc(data_corpus_irishbudget2010)}. See \code{\link{[.corpus}} for 
#'   details.
#'   
#'   Assigning docvars to a \link{tokens} object is not supported.  (You should
#'   only be manipulating these variables at the corpus level.)
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' # assigning document variables to a corpus
#' docvars(data_corpus_inaugural, "President") <- paste("prez", 1:ndoc(data_corpus_inaugural), sep = "")
#' head(docvars(data_corpus_inaugural))
#' 
#' # alternative using indexing
#' head(data_corpus_inaugural[, "Year"])
#' data_corpus_inaugural[["President2"]] <- paste("prezTwo", 1:ndoc(data_corpus_inaugural), sep = "")
#' head(docvars(data_corpus_inaugural))
#' 
#' @export
"docvars<-" <- function(x, field = NULL, value) {
    UseMethod("docvars<-")
}


#' @noRd
#' @export
"docvars<-.corpus" <- function(x, field = NULL, value) {
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    if (is.null(field)) {
        field <- names(value)
        if (is.null(field))
            field <- paste("docvar", 1:ncol(as.data.frame(value)), sep="")
    }
    documents(x)[field] <- value
    x
}

## internal only
"docvars<-.tokens" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        attr(x, "docvars") <- value
    } else {
        if (!is.data.frame(attr(x, "docvars"))) {
            meta <- data.frame(value, stringsAsFactors = FALSE)
            colnames(meta) <- field
            attr(x, "docvars") <- meta
        } else {
            attr(x, "docvars")[[field]] <- value
        }
    }
    return(x)
}

## internal only
"docvars<-.dfm" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        x@docvars <- value
    } else {
        if (!is.data.frame(x@docvars)) {
            meta <- data.frame(value, stringsAsFactors = FALSE)
            colnames(meta) <- field
            x@docvars <- meta
        } else {
            x@docvars[[field]] <- value
        }
    }
    return(x)
}

#' get or set document-level meta-data
#' 
#' Get or set the document-level meta-data.
#' @param x a \link{corpus} object
#' @param field character, the name of the metadata field(s) to be queried or set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_language}, but when named in in the \code{field} argument,
#'   do \emph{not} need the underscore character.
#' @examples 
#' mycorp <- corpus_subset(data_corpus_inaugural, Year > 1990)
#' summary(mycorp, showmeta = TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta = TRUE)
#' @export
#' @keywords corpus
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")


#' @noRd
#' @export
metadoc.corpus <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- documents(x)[, which(substring(names(documents(x)), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @noRd
#' @export
metadoc.tokens <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- attr(x, "docvars")[, which(substring(names(attr(x, "docvars")), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @noRd
#' @export
metadoc.dfm <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- x@docvars[, which(substring(names(x@docvars), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @rdname metadoc
#' @param value the new value of the new meta-data field
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc")

#' @noRd
#' @export
"metadoc<-" <- function(x, field = NULL, value) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep="")
        if (is.null(field))
            field <- paste("_metadoc", 1:ncol(as.data.frame(value)), sep = "")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(x)[field] <- value
    x
}

## helper function to check fields and report error message if
## a field is not a valid docvar name
check_fields <- function(x, field = NULL) {
    if (!is.null(field)) {
        if (length(notin <- which(! field %in% c(names(docvars(x)), names(metadoc(x))))))
            stop("field(s) ", field[notin], " not found", call. = FALSE)
    }
}

