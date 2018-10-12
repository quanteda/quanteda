
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
    x <- as.corpus(x)
    txt <- as.character(unclass(x))
    
    # without groups
    if (is.null(groups)) {
        names(txt) <- docnames(x)
        return(txt)
    }
    
    if (is.character(groups) & all(groups %in% attr(x, "docvars"))) {
        if (any(is_internal(groups)))
            message_error("docvar_invalid")
        group <- as.factor(interaction(attr(x, "docvars")[groups]))
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
        group <- as.factor(groups)
    }
    
    texts(txt, groups = group, spacer = spacer)
}

#' @noRd
#' @export
texts.character <- function(x, groups = NULL, spacer = "  ") {
    if (is.null(groups)) return(x)
    stri_c_list(split(texts(x), as.factor(groups)), collapse = spacer)
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
#' corp <- corpus(c("We must prioritise honour in our neighbourhood.", 
#'                  "Aluminium is a valourous metal."))
#' texts(corp) <- 
#'     stringi::stri_replace_all_regex(texts(corp),
#'                                    c("ise", "([nlb])our", "nium"),
#'                                    c("ize", "$1or", "num"),
#'                                    vectorize_all = FALSE)
#' texts(corp)
#' texts(corp)[2] <- "New text number 2."
#' texts(corp)
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @noRd
#' @export
"texts<-.corpus" <- function(x, value) {
    x <- as.corpus(x)
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    return(x)
}

#' @rdname texts
#' @details \code{as.character(x)} where \code{x} is a corpus is equivalent to
#' calling \code{texts(x)}
#' @param ... unused
#' @method as.character corpus
#' @return \code{as.character(x)} is equivalent to \code{texts(x)}
#' @export
as.character.corpus <- function(x, ...) {
    x <- as.corpus(x)
    texts(x)
}

#' coerce a compressed corpus to a standard corpus
#' 
#' Recast a compressed corpus object into a standard (uncompressed) corpus
#' object.
#' @param x a compressed \link{corpus} object
#' @export
#' @keywords internal
as.corpus <- function(x) {
    UseMethod("as.corpus")
}

#' @export
as.corpus.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.corpus"))
}

#' @export
#' @method as.corpus corpus
as.corpus.corpus <- function(x) {
    
    if (is.character(x) && is.data.frame(attr(x, "docvars")))
        return(x)
    
    # drop internal variables
    flag <- is_internal(names(x$documents))
    vars_internal <- x$documents[flag]
    x$documents <- x$documents[!flag]
    
    result <- corpus(x$documents, "row.names", "texts")
    
    # overwite internal variables
    vars <- attr(result, "docvars")
    if ("_document" %in% names(vars_internal)) {
        vars["_docname"] <- vars_internal["_document"]
    }
    if ("_docid" %in% names(vars_internal)) {
        vars["_docnum"] <- vars_internal["_docid"]
    }
    if ("_segid" %in% names(vars_internal)) {
        vars["_segnum"] <- vars_internal["_segid"]
    }
    
    attr(result, "docvars") <- vars
    attr(result, "meta")$created <- as.POSIXct(x$metadata$created, 
                                               format = "%a %b %d %H:%M:%S %Y")
    return(result)
}


#' @export
#' @method as.corpus corpuszip
as.corpus.corpuszip <- function(x) {
    
    txt <- memDecompress(x$texts, 'gzip', asChar = TRUE)
    txt <- strsplit(txt, paste0("###END_DOCUMENT###", "\n"))
    txt <- unlist(txt, use.names = FALSE)
    
    # drop internal variables
    flag <- is_internal(names(x$documents))
    corpus(txt, x$docnames, docvars = x$documents[!flag])
}

