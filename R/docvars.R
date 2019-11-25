# docvars ------

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
    check_fields(x, field)
    if (is_pre2(x)) {
        dvars <- documents(x)[, which(names(documents(x)) != "texts"), drop = FALSE]
        if (is.null(field))
            dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
        return(get_docvars(dvars, field))
    } else {
        dvars <- docvarsv2.corpus(x)
        rownames(dvars) <- docnamesv2.corpus(x)
        return(dvars)
    }
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    check_fields(x, field)
    dvars <- attr(x, "docvars")
    if (is.null(field))
        dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
    get_docvars(dvars, field) 
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    check_fields(x, field)
    dvars <- x@docvars
    if (is.null(field))
        dvars <- dvars[, which(substring(names(dvars), 1, 1) != "_"), drop = FALSE]
    get_docvars(dvars, field) 
}

#' @noRd
#' @keywords internal
docvars.kwic <- function(x) {
    dvars <- attr(x, "docvars")
    if (is.null(dvars))
        dvars <- data.frame()
    dvars <- structure(dvars[attr(x, "docid"), ],
                       class = "data.frame",
                       row.names = paste(x$docname, attr(x, "segid"), sep = "."))
    select_fields(dvars)
}

# docvars<- ------

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
    x <- corpus(x)
    x$documents <- set_docvars(x$documents, field, value)
    return(x)
}

#' @export
"docvars<-.tokens" <- function(x, field = NULL, value) {
    attr(x, "docvars") <- set_docvars(attr(x, "docvars"), field, value)
    return(x)
}

#' @export
"docvars<-.dfm" <- function(x, field = NULL, value) {
    x@docvars <- set_docvars(x@docvars, field, value)
    return(x)
}

# metadoc -------

#' Get or set document-level meta-data
#' 
#' @description
#' Get or set document-level meta-data.  Document-level meta-data are a special 
#' type of \link{docvars}, meant to contain information about documents that 
#' would not be used as a "variable" for analysis. An example could be the 
#' source of the document, or notes pertaining to its transformation, copyright 
#' information, etc.
#' 
#' Document-level meta-data differs from corpus-level meta-data in that the 
#' latter pertains to the collection of texts as a whole, whereas the 
#' document-level version can differ with each document.
#' @param x a \link{corpus} object
#' @param field character, the name of the metadata field(s) to be queried or 
#'   set
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
#' @seealso \code{\link{metacorpus}}
#' @export
#' @keywords corpus
metadoc <- function(x, field = NULL)
    UseMethod("metadoc")

#' @export
metadoc.default <- function(x, field = NULL) {
    stop(friendly_class_undefined_message(class(x), "metadoc"))
}

#' @noRd
#' @export
metadoc.corpus <- function(x, field = NULL) {
    x <- corpus(x)
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- documents(x)[, which(substring(names(documents(x)), 1, 1) == "_"),
                          drop = FALSE]
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
    UseMethod("metadoc<-")

#' @export
"metadoc<-.default" <- function(x, field = NULL, value) {
    stop(friendly_class_undefined_message(class(x), "metadoc<-"))
}

#' @noRd
#' @export
"metadoc<-.corpus" <- function(x, field = NULL, value) {
    x <- corpus(x)
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep = "")
        if (is.null(field))
            field <- paste("_metadoc", seq_len(ncol(as.data.frame(value))), sep = "")
    } else {
        field <- paste("_", field, sep = "")
    }
    documents(x)[field] <- value
    x
}

# Internal functions -----

## internal function to return the docvars for all docvars functions
get_docvars <- function(dvars, field = NULL) {
    if (is.null(field)) {
        if (is.null(dvars)) {
            result <- data.frame()
        } else {
            result <- dvars
        }
    } else {
        result <- dvars[, field, drop = TRUE]
    }
    return(result)
}

set_docvars <- function(dvars, field = NULL, value)  {
    if (is.null(dvars) && is.data.frame(value)) {
        result <- value
    } else {
        dvars_system <- select_fields(dvars, c("text", "system"))
        dvars_user <- select_fields(dvars, "user")
        
        if (is.dfm(value))
            value <- convert(value, to = "data.frame")[, -1, drop = FALSE]
        
        if (is.null(field)) {
            if (is.null(value)) {
                result <- dvars_system
            } else {
                if (is.matrix(value))
                    value <- as.data.frame(value, stringsAsFactors = FALSE)
                if (is.data.frame(value)) {
                    if (nrow(value) != nrow(dvars))
                        stop(message_error("docvar_mismatch"), call. = FALSE)
                    if (!is.character(names(value)) || length(names(value)) != ncol(value) || 
                        any(is.na(names(value)))) {
                        stop(message_error("docvar_nocolname"), call. = FALSE)
                    }
                    result <- cbind(dvars_system, value)
                } else {
                    stop(message_error("docvar_nofield"), call. = FALSE)
                }
            }
        } else {
            if ("texts" %in% field)
                stop("You should use texts", call. = FALSE)
            dvars_user[field] <- value
            result <- cbind(dvars_system, dvars_user)
        }
    }
    if (ncol(result) == 0)
        names(result) <- character() # cbind() sets NULL to names
    return(result)
}

## helper function to check fields and report error message if
## a field is not a valid docvar name
check_fields <- function(x, field = NULL) {
    if (!is.null(field)) {
        if (length(notin <- which(!field %in% c(names(docvars(x)), names(metadoc(x))))))
            stop("field(s) ", field[notin], " not found", call. = FALSE)
    }
}

# new docvar functions

## helper function to get all docvars
docvars_internal <- function(x) {
    if (is.corpus(x)) {
        return(documents(x))
    } else if (is.tokens(x)) {
        return(attr(x, "docvars"))
    } else if (is.dfm(x)) {
        return(x@docvars)
    }
}

## helper function to check fields
check_docvars <- function(x, fields) {
    dvars <- docvars_internal(x)
    if (is.null(dvars))
        return(rep(FALSE, length(fields)))
    return(fields %in% names(dvars))
}

## internal function to select docvar fields
select_fields <- function(x, types = c("user", "system")) {

    names <- names(x)
    is_system <- stri_startswith_fixed(names, "_")
    is_text <- stri_detect_fixed(names, "texts") | stri_detect_fixed(names, "_texts")

    result <- data.frame(row.names = row.names(x))
    if ("text" %in% types) {
        result <- cbind(result, x[is_text])
    }
    if ("system" %in% types) {
        result <- cbind(result, x[is_system & !is_text])
    }
    if ("user" %in% types) {
        result <- cbind(result, x[!is_system & !is_text])
    }
    if (ncol(result) == 0)
        names(result) <- character() # cbind() sets NULL to names
    return(result)
}
