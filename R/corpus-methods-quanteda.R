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
    x <- corpus(x)
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
    x <- corpus(x)
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    x$metadata[field] <- value
    x
}


# texts() functions ----------------------------

#' Get or assign corpus texts
#' 
#' Get or replace the texts in a \link{corpus}, with grouping options. 
#' Works for plain character vectors too, if \code{groups} is a factor.
#' @note The \code{groups} will be used for concatenating the texts based on shared
#' values of \code{groups}, without any specified order of aggregation.
#' @param x a \link{corpus} or character object
#' @inheritParams groups
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
#' nchar(data_char_ukimmig2010[1:5])
#' nchar(texts(data_corpus_inaugural[1:5], 
#'             groups = as.factor(data_corpus_inaugural[1:5, "President"])))
#' 
texts <- function(x, groups = NULL, spacer = "  ") {
    UseMethod("texts")
}

#' @noRd
#' @export
texts.corpus <- function(x, groups = NULL, spacer = "  ") {
    x <- corpus(x)
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
    vapply(x, paste, character(1), collapse = spacer)
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
#'   \code{\link{tokens_tolower}} after applying \code{\link{tokens}} to a
#'   corpus, or use the option \code{tolower = TRUE} in \code{\link{dfm}}.
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
    x <- corpus(x)
    documents(x)$texts <- value
    x
}

#' @rdname texts
#' @details \code{as.character(x)} where \code{x} is a corpus is equivalent to
#' calling \code{texts(x)}
#' @param ... unused
#' @method as.character corpus
#' @return \code{as.character(x)} is equivalent to \code{texts(x)}
#' @export
as.character.corpus <- function(x, ...) {
    texts(x)
}

# internal: documents() functions ---------------------------------

# internal accessor for documents object
# @export
documents <- function(x) {
    UseMethod("documents")
}

documents.corpus <- function(x) {
    x <- corpus(x)
    x$documents
}

documents.tokens <- function(x) {
    docvars(x)
}

documents.dfm <- function(x) {
    docvars(x)
}

# internal replacement function for documents
"documents<-" <- function(x, value) {
    UseMethod("documents<-")
}

"documents<-.corpus" <- function(x, value) {
    x$documents <- value
    x
}

