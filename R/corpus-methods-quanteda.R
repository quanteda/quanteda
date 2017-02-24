#' get or set corpus metadata
#' 
#' Get or set the corpus-level metadata in a quanteda corpus object.
#' 
#' @param x A quanteda corpus object
#' @param field Metadata field name(s).  If \code{NULL} (default), return all
#'   metadata names.
#' @return For \code{metacorpus}, a list of the metadata fields in the corpus. 
#'   If a list is not what you wanted, you can wrap the results in \link{unlist}, 
#'   but this will remove any metadata field that is set to \code{NULL}.
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

# replacement function for corpus-level data
#' @param value new value of the corpus metadata field
#' @export
#' @rdname metacorpus
"metacorpus<-" <- function(x, field, value) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    x$metadata[field] <- value
    x
}


# internal accessor for documents object
# @export
documents <- function(corp) {
    corp$documents
}

# internal replacement function for documents
# @export
"documents<-" <- function(corp, value) {
    corp$documents <- value
    corp
}


#' get or assign corpus texts
#' 
#' Get or replace the texts in a \link{corpus} object, with grouping options. 
#' Works for plain character vectors too, if \code{groups} is a factor.
#' @param x a quanteda \link{corpus} or character object
#' @param groups either: a character vector containing the names of document
#'   variables to be used for grouping; or a factor (or object that can be
#'   coerced into a factor) equal in length to the number of documents, used for
#'   aggregating the texts through concatenation
#' @param spacer when concatenating texts by using \code{groups}, this will be the 
#'   spacing added between texts.  (Default is two spaces.)
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @export
#' @keywords corpus
#' @examples
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806)))
#' 
#' # grouping on a document variable
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806), groups = "President"))
#' 
#' # grouping a character vector using a factor
#' nchar(data_char_inaugural[1:5])
#' nchar(texts(data_char_inaugural[1:5], 
#'             groups = as.factor(data_corpus_inaugural[1:5, "President"])))
#' 
texts <- function(x, groups = NULL, spacer = "  ") {
    UseMethod("texts")
}

#' @noRd
#' @export
texts.corpus <- function(x, groups = NULL, spacer = "  ") {
    txts <- documents(x)$texts
    
    # without groups
    if (is.null(groups)) {
        names(txts) <- docnames(x)
        return(txts)
    }
    
    if (is.character(groups) & all(groups %in% names(documents(x)))) {
        group.split <- as.factor(interaction(documents(x)[, groups], drop = TRUE))
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
        group.split <- as.factor(groups)
    }
    
    texts(txts, groups = group.split, spacer = spacer)
}

#' @noRd
#' @export
texts.character <- function(x, groups = NULL, spacer = "  ") {
    if (is.null(groups)) return(x)
    # if (!is.factor(groups)) stop("groups must be a factor")
    x <- split(x, as.factor(groups))
    sapply(x, paste, collapse = spacer)
}


#' @rdname texts
#' @param value character vector of the new texts
#' @return for \code{texts <-}, a corpus with the texts replaced by \code{value}
#' @export
#' @note You are strongly encouraged as a good practice of text analysis 
#'   workflow \emph{not} to modify the substance of the texts in a corpus. 
#'   Rather, this sort of processing is better performed through downstream 
#'   operations.  For instance, do not lowercase the texts in a corpus, or you 
#'   will never be able to recover the original case.  Rather, apply 
#'   \code{\link{toLower}} to the corpus and use the result as an input, e.g. to
#'   \code{\link{tokenize}}.
#' @examples
#' BritCorpus <- corpus(c("We must prioritise honour in our neighbourhood.", 
#'                        "Aluminium is a valourous metal."))
#' texts(BritCorpus) <- 
#'     stringi::stri_replace_all_regex(texts(BritCorpus),
#'                                    c("ise", "([nlb])our", "nium"),
#'                                    c("ize", "$1or", "num"),
#'                                    vectorize_all = FALSE)
#' texts(BritCorpus)
#' texts(BritCorpus)[2] <- "New text number 2."
#' texts(BritCorpus)
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @noRd
#' @export
"texts<-.corpus" <- function(x, value) { 
    documents(x)$texts <- value
    x
}

#' @rdname texts
#' @details \code{as.character(x)} where \code{x} is a corpus is equivalent to
#' calling \code{texts(x)}
#' @method as.character corpus
#' @return \code{as.character(x)} is equivalent to \code{texts(x)}
#' @export
as.character.corpus <- function(x, ...) {
    texts(x)
}


#' get or set document-level meta-data
#' 
#' Get or set the document-level meta-data, including reserved fields for 
#' language and corpus.
#' @param x A quanteda corpus object
#' @param field character, the name of the metadata field(s) to be queried or set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_language}, but when named in in the \code{field} argument,
#'   do \emph{not} need the underscore character.
#' @examples 
#' mycorp <- corpus_subset(data_corpus_inaugural, Year>1990)
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
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (length(field) > 1)
        stop("cannot assign multiple fields.")
    if (is.null(field)) {
        documents(x)[, grep("^\\_", names(documents(x))), drop = FALSE]
    } else {
        ## error if field not defined in data
        fieldname <- ifelse(substr(field, 1, 1)=="_", 
                            field, 
                            paste("_", field, sep=""))
        documents(x)[, fieldname, drop = TRUE]
    }
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
            field <- paste("_metadoc", 1:ncol(as.data.frame(value)), sep="")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(x)[field] <- value
    x
}


#' get or set for document-level variables
#' 
#' Get or set variables for the documents in a corpus
#' @param x corpus whose document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables
#' @examples head(docvars(data_corpus_inaugural))
#' @export
#' @keywords corpus
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

#' @noRd
#' @export
docvars.corpus <- function(x, field = NULL) {
    # allow metadoc _variables to be returned, if named
    docvarsIndex <- 
        union(which(names(documents(x)) == field[substr(field, 1, 1) == "_"]),
              intersect(which(substr(names(documents(x)), 1, 1) != "_"),
                        which(names(documents(x)) != "texts")))
    
    if (length(docvarsIndex) == 0)
        return(NULL)
    if (is.null(field))
        return(documents(x)[, docvarsIndex, drop = FALSE])
    return(documents(x)[, field, drop = TRUE])
}

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Another way to access and set docvars is through indexing of the corpus \code{j} element, 
#' such as \code{data_corpus_irishbudget2010[, c("foren", "name"]} or for a single docvar, \code{data_corpus_irishbudget2010[["name"]]}.  The latter
#' also permits assignment, including the easy creation of new document varibles, e.g. \code{data_corpus_irishbudget2010[["newvar"]] <- 1:ndoc(data_corpus_irishbudget2010)}.
#' See \code{\link{[.corpus}} for details.
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' docvars(data_corpus_inaugural, "President") <- paste("prez", 1:ndoc(data_corpus_inaugural), sep="")
#' head(docvars(data_corpus_inaugural))
#' 
#' # alternative using indexing
#' head(data_corpus_inaugural[, "Year"])
#' data_corpus_inaugural[["President2"]] <- paste("prezTwo", 1:ndoc(data_corpus_inaugural), sep="")
#' head(docvars(data_corpus_inaugural))
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


#' get or set document names
#' 
#' Get or set the document names of a \link{corpus}, \link{tokens}, or \link{dfm} object.
#' @param x the object with docnames
#' @export
#' @return \code{docnames} returns a character vector of the document names
#' @seealso \code{\link{featnames}}
#' @examples
#' # query the document names of a corpus
#' docnames(data_corpus_irishbudget2010)
#' 
#' # query the document names of a tokens object
#' docnames(tokens(data_char_ukimmig2010))
#' 
#' # query the document names of a dfm
#' docnames(dfm(data_char_inaugural[1:5]))
#' 
#' @keywords corpus dfm
docnames <- function(x) {
    UseMethod("docnames")
}

#' @noRd
#' @export
docnames.corpus <- function(x) {
    # didn't use accessor documents() because didn't want to pass
    # that large object
    rownames(x$documents)
}

#' @param value a character vector of the same length as \code{x}
#' @return \code{docnames <-} assigns new values to the document names of a corpus. (Does not work
#' for dfm objects, whose document names are fixed.)
#' @export
#' @examples 
#' # reassign the document names of the inaugural speech corpus
#' docnames(data_corpus_inaugural) <- paste("Speech", 1:ndoc(data_corpus_inaugural), sep="")
#' 
#' @rdname docnames
"docnames<-" <- function(x, value) {
    UseMethod("docnames<-")
}

#' @noRd
#' @export
"docnames<-.corpus" <- function(x, value) {
    if (!is.corpus(x))
        stop("docnames<-  only valid for corpus objects.")
    rownames(x$documents) <- value
    return(x)
}

