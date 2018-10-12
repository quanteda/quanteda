# metacorpus functions ---------------------

#' Get or set corpus metadata
#' 
#' Get or set the corpus-level metadata in a \link{corpus} object.
#' @param x a \link{corpus} object
#' @param field metadata field name(s);  if \code{NULL} (default), return all 
#'   metadata names
#' @return For \code{metacorpus}, a named list of the metadata fields in the corpus. 
#'   
#'   For \code{metacorpus <-}, the corpus with the updated metadata.
#' @export
#' @keywords corpus
#' @examples
#' metacorpus(data_corpus_inaugural)
#' metacorpus(data_corpus_inaugural, "source")
#' metacorpus(data_corpus_inaugural, "citation") <- "Presidential Speeches Online Project (2014)."
#' metacorpus(data_corpus_inaugural, "citation")
metacorpus <- function(x, field = NULL)
    UseMethod("metacorpus")

#' @export
metacorpus.default <- function(x, field = NULL) {
    stop(friendly_class_undefined_message(class(x), "metacorpus"))
}

#' @noRd
#' @export
metacorpus.corpus <- function(x, field = NULL) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
        return(x$metadata[field])
    } else {
        return(x$metadata)
    }
}

#' Replacement function for corpus-level data
#' @param value new value of the corpus metadata field
#' @export
#' @rdname metacorpus
"metacorpus<-" <- function(x, field, value) {
    UseMethod("metacorpus<-")
}

#' @export
"metacorpus<-.default" <- function(x, field, value) {
    stop(friendly_class_undefined_message(class(x), "metacorpus<-"))
}

#' @export
"metacorpus<-.corpus" <- function(x, field, value) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    x$metadata[field] <- value
    x
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
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep="")
        if (is.null(field))
            field <- paste("_metadoc", seq_len(ncol(as.data.frame(value))), sep = "")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(x)[field] <- value
    x
}