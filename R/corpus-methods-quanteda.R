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
#' @examples
#' metacorpus(data_corpus_inaugural)
#' metacorpus(data_corpus_inaugural, "source")
#' metacorpus(data_corpus_inaugural, "citation") <- "Presidential Speeches Online Project (2014)."
#' metacorpus(data_corpus_inaugural, "citation")
metacorpus <- function(x, field = NULL)
    UseMethod("metacorpus")

#' @rdname metacorpus
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


#' get corpus texts
#' 
#' Get the texts in a quanteda corpus object, with grouping options.  Works for plain character
#' vectors too, if \code{groups} is a factor.
#' @param x A quanteda corpus object
#' @param groups character vector containing the names of document variables in
#'   a corpus, or a factor equal in length to the number of documents, used for 
#'   aggregating the texts through concatenation.  If \code{x} is of type character,
#'   then \code{groups} must be a factor.
#' @param ... unused
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @export
#' @examples
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806)))
#' 
#' # grouping on a document variable
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806), groups = "President"))
#' 
#' # grouping a character vector using a factor
#' nchar(data_char_inaugural[1:5])
#' nchar(texts(data_char_inaugural[1:5], groups = as.factor(data_corpus_inaugural[1:5, "President"])))
texts <- function(x, groups = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    UseMethod("texts")
}

#' @rdname texts
#' @export
texts.corpus <- function(x, groups = NULL, ...) {
    txts <- documents(x)$texts
    
    # without groups
    if (is.null(groups)) {
        names(txts) <- docnames(x)
        return(txts)
    }
    
    # with groups as a factor
    if (any(!groups %in% names(docvars(x)))) {
        stop("Check your docvar names.", 
             ifelse(length(groups) == ndoc(x), "  Try groups = as.factor()...", ""))
    }
    if (is.factor(groups)) {
        group.split <- groups
    } else {
        #        if (length(groups) > 1) {
        #            # if more than one grouping variable
        group.split <- as.factor(interaction(documents(x)[, groups], drop = TRUE))
        #        } else {
        #            # if only one grouping variable
        #            group.split <- as.factor(documents(x)[, groups])
        #        }
    }
    texts(txts, groups = group.split)
}

#' @rdname texts
#' @export
texts.character <- function(x, groups = NULL, ...) {
    if (is.null(groups)) return(x)
    if (!is.factor(groups)) stop("groups must be a factor")
    x <- split(x, groups)
    sapply(x, paste, collapse = " ")
}


#' @param value character vector of the new texts
#' @rdname texts
#' @export
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @rdname texts
#' @export
#' @note You are strongly encouraged as a good practice of text analysis 
#'   workflow \emph{not} to modify the substance of the texts in a corpus. 
#'   Rather, this sort of processing is better performed through downstream 
#'   operations.  For instance, do not lowercase the texts in a corpus, or you 
#'   will never be able to recover the original case.  Rather, apply 
#'   \code{\link{toLower}} to the corpus and use the result as an input, e.g. to
#'   \code{\link{tokenize}}.
#' @examples 
#' 
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
"texts<-.corpus" <- function(x, value) { 
    documents(x)$texts <- value
    x
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
#' @export
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")

#' @rdname metadoc 
#' @export
#' @examples
#' mycorp <- corpus_subset(data_corpus_inaugural, Year>1990)
#' summary(mycorp, showmeta = TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta = TRUE)
metadoc.corpus <- function(x, field = NULL) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (length(field) > 1)
        stop("cannot assign multiple fields.")
    if (is.null(field)) {
        documents(x)[, grep("^\\_", names(documents(x))), drop=FALSE]
    } else {
        ## error if field not defined in data
        fieldname <- ifelse(substr(field, 1, 1)=="_", 
                            field, 
                            paste("_", field, sep=""))
        documents(x)[, fieldname, drop=FALSE]
    }
}

#' @param value the new value of the new meta-data field
#' @rdname metadoc
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc")

#' @rdname metadoc
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

# replacement function for document-level metadata
#
# to get this to work with indexes, e.g. 
# metadoc(UDHRcorpus, "language")[1] <- "1st Row Only"
# or
# language(UDHRcorpus)[1] <- "1st row only"
# is trickier.  Solution lies in nesting a complex "[" function
# inside the calling function: see http://cran.r-project.org/doc/manuals/R-lang.html#Subset-assignment
#
# @export
# "metadoc<-[" <- function(corp, value, field) {
#     # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
#     # (this check not yet implemented)
#     field <- paste("_", field, sep="")
#     documents(corp)[field] <- value
#     corp
# }



#' get or set for document-level variables
#' 
#' Get or set variables for the documents in a corpus
#' @param x corpus whose document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables
#' @examples head(docvars(data_corpus_inaugural))
#' @export
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

#' @rdname docvars
#' @export
docvars.corpus <- function(x, field = NULL) {
    docvarsIndex <- intersect(which(substr(names(documents(x)), 1, 1) != "_"),
                              which(names(documents(x)) != "texts"))
    if (length(docvarsIndex)==0)
        return(NULL)
    if (is.null(field))
        return(documents(x)[, docvarsIndex, drop=FALSE])
    return(documents(x)[, field, drop=TRUE])
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


#' @rdname docvars
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


# accessor for tokens
# 
# Get the tokens object from a corpus
# @export
#  return(corp$docvars$tokens)
tokens <- function(x) {
    UseMethod("tokens")
}
tokens.corpus <- function(corp) {
    corp$tokens
}

# # replacement function for tokens
# # @export
#  corp$docvars$tokens <- value
#  return(corp)
# #"tokens<-" <- function(corp, value){
#}
# # @export
#types <- function(corp) {
#  return(unique(unlist(tokens(corp))))
#}

#' get or set document names
#' 
#' Get or set the document names from a corpus or a document-feature matrix.
#' of the \link{dfm} object.
#' @param x the object with docnames
#' @export
docnames <- function(x) {
    UseMethod("docnames")
}

#' @return \code{docnames} returns a character vector of the document names
#' @export
#' @rdname docnames
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
#' # query the document names of the inaugural speech corpus
#' docnames(data_corpus_inaugural) <- paste("Speech", 1:ndoc(data_corpus_inaugural), sep="")
#' 
#' # reassign the document names of the inaugural speech corpus
#' docnames(data_corpus_inaugural) <- paste("Speech", 1:ndoc(data_corpus_inaugural), sep="")
#' 
#' @rdname docnames
"docnames<-" <- function(x, value) {
    if (!is.corpus(x))
        stop("docnames<-  only valid for corpus objects.")
    rownames(x$documents) <- value
    return(x)
}

#' get the number of documents or features
#' 
#' \code{ndoc} returns the number of documents or features in a quanteda object, which can be 
#' a corpus, dfm, or tokenized texts.
#' @param x a corpus or dfm object
#' @return an integer (count) of the number of documents or features in the corpus or dfm
#' @export
ndoc <- function(x) {
    UseMethod("ndoc")
}

#' @rdname ndoc
#' @examples 
#' ndoc(corpus_subset(data_corpus_inaugural, Year>1980))
#' ndoc(dfm(corpus_subset(data_corpus_inaugural, Year>1980), verbose=FALSE))
#' @export
ndoc.corpus <- function(x) {
    nrow(x$documents)
}

# # get or set the language of corpus documents
# # 
# # Get or set the \code{_language} document-level metadata field in a corpus.
# # @param corp a corpus object
# # @param drop return as a vector if \code{TRUE}, otherwise return a \code{data.frame}
# # @details This function modifies the \code{_language} value set by
# #   \code{\link{metadoc}}.  It is a wrapper for \code{metadoc(corp, "language")}.
# # @export
# language <- function(corp, drop=TRUE) {
#     if ("_language" %in% names(metadoc(corp))) {
#         result <- metadoc(corp, "language")
#         return(result[,1, drop=drop])
#     } else
#         return(rep(NULL, ndoc(corp)))
# }
# 
# # @rdname language
# # @param value the new value for the language meta-data field, a string or
# #   character vector equal in length to \code{ndoc(corp)}
# # @export
# "language<-" <- function(corp, value){
#     metadoc(corp, "language") <- value
#     # corp$documents$"_language" <- value
#     corp
# }



#' Randomly sample documents or features
#' 
#' Takes a random sample or documents or features of the specified size from a 
#' corpus or document-feature matrix, with or without replacement
#' 
#' @param x a corpus or dfm object whose documents or features will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param ... unused
#'   \code{\link[base]{sample}}, which is not defined as a generic
#'   method in the \pkg{base} package.
#' @export
sample <- function(x, size, replace = FALSE, prob = NULL, ...) {
    UseMethod("sample")
}

#' @export
#' @rdname sample
sample.default <- function(x, size, replace = FALSE, prob = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    base::sample(x, size, replace, prob)
}


#' @export
#' @return A corpus object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @seealso \code{\link{sample}}
#' @rdname sample
#' @examples
#' # sampling from a corpus
#' summary(sample(data_corpus_inaugural, 5)) 
#' summary(sample(data_corpus_inaugural, 10, replace=TRUE))
sample.corpus <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, ...) {
    documents(x) <- documents(x)[sample(ndoc(x), size, replace, prob), , drop = FALSE]
    x
}


#' change the document units of a corpus
#' 
#' For a corpus, recast the documents down or up a level of aggregation.  "Down"
#' would mean going from documents to sentences, for instance.  "Up" means from 
#' sentences back to documents.  This makes it easy to reshape a corpus from a 
#' collection of documents into a collection of sentences, for instance.
#' (Because the corpus object records its current "units" status, there is no 
#' \code{from} option, only \code{to}.)
#' @param x corpus whose document units will be reshaped
#' @param to new documents units for the corpus to be recast in
#' @param ... not used
#' @export
changeunits <- function(x, ...) 
    UseMethod("changeunits")

#' @rdname changeunits
#' @return A corpus object with the documents defined as the new units,
#'   including document-level meta-data identifying the original documents.
#' @export
#' @examples
#' # simple example
#' mycorpus <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                      textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                    docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
#'                    notes = "This is a simple example to show how changeunits() works.")
#' summary(mycorpus)
#' summary(changeunits(mycorpus, to = "sentences"), showmeta=TRUE)
#' 
#' # example with inaugural corpus speeches
#' (mycorpus2 <- corpus_subset(data_corpus_inaugural, Year>2004))
#' paragCorpus <- changeunits(mycorpus2, to="paragraphs")
#' paragCorpus
#' summary(paragCorpus, 100, showmeta=TRUE)
#' ## Note that Bush 2005 is recorded as a single paragraph because that text used a single
#' ## \n to mark the end of a paragraph.
changeunits.corpus <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    to <- match.arg(to)
    if (to == "documents") stop("documents not yet implemented.")
    
    if (length(addedArgs <- names(list(...))))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    # make the new corpus
    segmentedTexts <- char_segment(texts(x), to)
    lengthSegments <- sapply(segmentedTexts, length)
    newcorpus <- corpus(unlist(segmentedTexts))
    # repeat the docvars and existing document metadata
    docvars(newcorpus, names(docvars(x))) <- as.data.frame(lapply(docvars(x), rep, lengthSegments))
    docvars(newcorpus, names(metadoc(x))) <- as.data.frame(lapply(metadoc(x), rep, lengthSegments))
    # add original document name as metadata
    metadoc(newcorpus, "document") <- rep(names(segmentedTexts), lengthSegments)
    # give a serial number (within document) to each sentence
    sentenceid <- lapply(lengthSegments, function(n) seq(from=1, to=n))
    metadoc(newcorpus, "serialno") <- unlist(sentenceid, use.names=FALSE)
    
    # copy settings and corpus metadata
    newcorpus$settings <- x$settings
    newcorpus$metadata <- x$metadata
    
    # modify settings flag for changeunits info
    settings(newcorpus, "unitsoriginal") <- settings(newcorpus, "units")
    settings(newcorpus, "units") <- to
    
    newcorpus
}

rep.data.frame <- function(x, ...)
    as.data.frame(lapply(x, rep, ...))



#' count the number of tokens or types
#' 
#' Return the count of tokens (total features) or types (unique features) in a
#' text, corpus, or dfm.  "tokens" here means all words, not unique words, and
#' these are not cleaned prior to counting.
#' @param x texts or corpus whose tokens or types will be counted
#' @param ... additional arguments passed to \code{\link{tokenize}}
#' @note Due to differences between raw text tokens and features that have been 
#'   defined for a \link{dfm}, the counts be different for dfm objects and the 
#'   texts from which the dfm was generated.  Because the method tokenizes the 
#'   text in order to count the tokens, your results will depend on the options 
#'   passed through to \code{\link{tokenize}}
#' @return scalar count of the total tokens or types
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
#' ntoken(txt)
#' ntype(txt)
#' ntoken(toLower(txt))  # same
#' ntype(toLower(txt))   # fewer types
#' ntoken(toLower(txt), removePunct = TRUE)
#' ntype(toLower(txt), removePunct = TRUE)
#' 
#' # with some real texts
#' ntoken(corpus_subset(data_corpus_inaugural, Year<1806, removePunct = TRUE))
#' ntype(corpus_subset(data_corpus_inaugural, Year<1806, removePunct = TRUE))
#' ntoken(dfm(corpus_subset(data_corpus_inaugural, Year<1800)))
#' ntype(dfm(corpus_subset(data_corpus_inaugural, Year<1800)))
#' @export
ntoken <- function(x, ...) {
    UseMethod("ntoken")
}

#' @rdname ntoken
#' @export
ntype <- function(x, ...) {
    UseMethod("ntype")
}

#' @rdname ntoken
#' @export
ntoken.corpus <- function(x, ...) {
    ntoken(texts(x), ...)
}

#' @rdname ntoken
#' @export
ntype.corpus <- function(x, ...) {
    ntype(texts(x), ...)
}


#' @rdname ntoken
#' @export
ntoken.character <- function(x, ...) {
    ntoken(tokenize(x, ...))
}

#' @rdname ntoken
#' @export
ntoken.tokenizedTexts <- function(x, ...) {
    lengths(x)
}

#' @rdname ntoken
#' @export
ntype.character <- function(x, ...) {
    ntype(tokenize(x, ...))
}


#' @rdname ntoken
#' @export
ntoken.dfm <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("additional arguments not used for ntoken.dfm()")
    rowSums(x)
}

#' @rdname ntoken
#' @export
ntype.dfm <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("additional arguments not used for ntoken.dfm()")
    # apply(x, 1, function(dfmrow) sum(dfmrow > 0))
    tmp <- sparseMatrix(i = x@i, p = x@p, x = x@x > 0, index1 = FALSE, dims = x@Dim)
    tmp <- rowSums(tmp)
    names(tmp) <- docnames(x)
    tmp
}

#' @rdname ntoken
#' @export
ntype.tokenizedTexts <- function(x, ...) {
    sapply(lapply(x, unique), length)
}


#' count the number of sentences
#' 
#' Return the count of sentences in a corpus or character.
#' @param x texts or corpus whose sentences will be counted
#' @param ... additional arguments passed to \code{\link{tokenize}}
#' @note `nsentence()` relies on the boundaries definitions in the \pkg{stringi}
#'   package (see \link[stringi]{stri_opts_brkiter}).  It does not count
#'   sentences correctly if the text has been transformed to lower case, and for
#'   this reason `nsentence()` will stop with an error if it detects all
#'   lower-cased text.
#' @return scalar count(s) of the total sentences per text
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence: second part of first sentence.",
#'          text2 = "A word. Repeated repeated.")
#' nsentence(txt)
#' @export
nsentence <- function(x, ...) {
    UseMethod("nsentence")
}

#' @rdname nsentence
#' @export
nsentence.character <- function(x, ...) {
    upcase <- try(any(stringi::stri_detect_charclass(x, "[A-Z]")), silent = TRUE)
    if (!is.logical(upcase)) {
        # warning("Input text contains non-UTF-8 characters.")
    }
    else if (!upcase)
        warning("nsentence() does not correctly count sentences in all lower-cased text")
    lengths(tokenize(x, what = "sentence", ...))
}

#' @rdname nsentence
#' @export
nsentence.corpus <- function(x, ...) {
    nsentence(texts(x), ...)
}
