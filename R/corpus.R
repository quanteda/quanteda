
#' constructor for corpus objects
#'   
#' Creates a corpus from a document source.  The current available
#'   document sources are: \itemize{ \item a character vector (as in R class
#'   \code{char}) of texts; \item a \link{corpusSource-class} object,
#'   constructed using \code{\link{textfile}}; \item a \pkg{tm}
#'   \link[tm]{VCorpus} class corpus object, meaning that anything you can use
#'   to create a \pkg{tm} corpus, including all of the tm plugins plus the 
#'   built-in functions of tm for importing pdf, Word, and XML documents, can be
#'   used to create a quanteda \link{corpus}. } Corpus-level meta-data can be
#'   specified at creation, containing (for example) citation information and
#'   notes, as can document-level variables and document-level meta-data.
#' @param x a source of texts to form the documents in the corpus, a character
#'   vector or a \link{corpusSource-class} object created using
#'   \code{\link{textfile}}.
#' @param ... additional arguments
#' @return A corpus class object containing the original texts, document-level 
#'   variables, document-level metadata, corpus-level metadata, and default 
#'   settings for subsequent processing of the corpus.  A corpus consists of a 
#'   list of elements described below, although these should only be accessed 
#'   through accessor and replacement functions, not directly (since the 
#'   internals may be subject to change).  The structure of a corpus classed 
#'   list object is:
#'   
#'   \item{$documents}{A data frame containing the document level information, 
#'   consisting of \code{\link{texts}}, user-named \code{\link{docvars}}
#'   variables describing attributes of the documents, and \code{metadoc}
#'   document-level metadata whose names begin with an underscore character,
#'   such as \code{_language}.}
#'   
#'   \item{$metadata}{A named list set of corpus-level meta-data, including 
#'   \code{source} and \code{created} (both generated automatically unless 
#'   assigned), \code{notes}, and \code{citation}.}
#'   
#'   \item{$settings}{Settings for the corpus which record options that govern 
#'   the subsequent processing of the corpus when it is converted into a 
#'   document-feature matrix (\link{dfm}).  See \link{settings}.}
#'   
#'   \item{$tokens}{An indexed list of tokens and types tabulated by document, 
#'   including information on positions.  Not yet fully implemented.}
#' @seealso \link{docvars}, \link{metadoc}, \link{metacorpus}, \link{language}, 
#'   \link{encoding}, \link{settings}, \link{texts}
#' @author Kenneth Benoit and Paul Nulty
#' @export
corpus <- function(x, ...) {
    UseMethod("corpus")
}

# @param docvarsfrom  Argument to specify where docvars are to be taken, from 
# parsing the filenames separated
# by \code{sep} or from meta-data embedded in the text file header (\code{headers}).
# @param docvarnames Character vector of variable names for \code{docvars}
# @param sep Separator if \code{\link{docvars}} names are taken from the filenames.
# @warning Only files with the extension \code{.txt} are read in using the directory method.
# @param pattern filename extension - set to "*" if all files are desired.  This is a 
# \link[=regex]{regular expression}.
# @rdname corpus
# @export
# @examples 
# \dontrun{
# # import texts from a directory of files
# summary(corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"), 
#                enc="UTF-8", docvarsfrom="filenames",
#                source="Ken's UK manifesto archive",
#                docvarnames=c("Country", "Level", "Year", "language")), 5))
# summary(corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"), 
#                enc="UTF-8", docvarsfrom="filenames",
#                source="Ken's UK manifesto archive",
#                docvarnames=c("Country", "Level", "Year", "language", "Party")), 5))
# 
# # choose a directory using a GUI
# corpus(directory())
#
# # from a zip file on the web
# myzipcorp <- corpus(zipfiles("http://kenbenoit.net/files/EUcoalsubsidies.zip"),
#                     notes="From some EP debate about coal mine subsidies")
# docvars(myzipcorp, speakername=docnames(myzipcorp))
# summary(myzipcorp)
# }
# corpus.directory <- function(x, enc=NULL, docnames=NULL, 
#                             docvarsfrom=c("none", "filenames", "headers"), 
#                             docvarnames=NULL, sep='_', pattern="\\.txt$",
#                             source=NULL, notes=NULL, citation=NULL, ...) {
#     if (class(x)[1] != "directory") stop("first argument must be a directory")
#     dvars <- NULL
#     docvarsfrom <- match.arg(docvarsfrom)
#     texts <- getTextDir(x, pattern=pattern)
#     fnames <- NULL
#     if (docvarsfrom == "filenames") {
#         fnames <- list.files(x, full.names=TRUE)
#         snames <- getRootFileNames(fnames)
#         snames <- gsub(".txt", "", snames)
#         parts <- strsplit(snames, sep)
#         if (var(sapply(parts, length)) != 0)
#             stop("Filename elements are not equal in length.")
#         dvars <-  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE), 
#                             stringsAsFactors=FALSE)
#         # assign default names in any case
#         names(dvars) <- paste("docvar", 1:ncol(dvars), sep="")  
#         if (!is.null(docvarnames)) {
#             names(dvars)[1:length(docvarnames)] <- docvarnames
#             if (length(docvarnames) != ncol(dvars)) {
#                 warning("Fewer docnames supplied than exist docvars - last ",
#                         ncol(dvars) - length(docvarnames), " docvars were given generic names.")
#             }
#         }
#         # remove the filename extension from the document names
#         names(texts) <- gsub(".txt", "", names(texts))
#     } else if (docvarsfrom == "headers") 
#         stop("headers argument not yet implemented.")
# 
# 
#     tmpCorp <- NextMethod(x=texts, enc=enc, docnames=docnames, docvars=dvars,
#                           source=source, notes=notes, citation=citation)
#     
#     # set document source as filename
#     if (!is.null(fnames)) {
#         metadoc(tmpCorp, "source") <- fnames
#     }
#     
#     tmpCorp
# }





# Corpus constructor for a character method
# 
# Details here.
# 
#' @param docnames Names to be assigned to the texts, defaults to the names of the 
#' character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param docvars A data frame of attributes that is associated with each text.
#' @param source A string specifying the source of the texts, used for referencing.
#' @param citation Information on how to cite the corpus.
#' @param notes A string containing notes about who created the text, warnings, To Dos, etc.
#' @param enc A string specifying the input encoding for texts in the 
#' corpus.  Must be a valid entry in \code{\link{iconvlist}()}, since the code in 
#' \code{corpus.character} will convert this to \code{UTF-8} using \code{\link{iconv}}.  
#' Currently only one input encoding can be specified for a collection of input texts, 
#' meaning that you should not mix input text encoding types in a single \code{corpus} call.
#' @rdname corpus
#' @export
#' @examples
#' #
#' # create a corpus from texts
#' corpus(inaugTexts)
#' 
#' # create a corpus from texts and assign meta-data and document variables
#' ukimmigCorpus <- corpus(ukimmigTexts, 
#'                             docvars=data.frame(party=names(ukimmigTexts)), 
#'                             enc="UTF-8") 
#'                             
corpus.character <- function(x, enc=NULL, docnames=NULL, docvars=NULL,
                             source=NULL, notes=NULL, citation=NULL, ...) {
    # name the texts vector
    if (!is.null(docnames)) {
        stopifnot(length(docnames)==length(x))
        names(x) <- docnames
    } else if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    # check validity of encoding label(s)
    if (!is.null(enc) && !(enc %in% iconvlist())) stop("enc argument not found in iconvlist()")
    
    # create document-meta-data
    if (is.null(source)) {
        source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
    }
    created <- date()
    metadata <- list(source=source, created=created, notes=notes, citation=citation)
    
    # create the documents data frame starting with the texts
    documents <- data.frame(texts=x, row.names=names(x),
                            check.rows=TRUE, stringsAsFactors=FALSE)

    
    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        stopifnot(nrow(docvars)==length(x))
        documents <- cbind(documents, docvars)
    } 

    # set the encoding label if specified
    if (!is.null(enc) && enc != "unknown") {
        documents$texts <- iconv(documents$texts, enc, "UTF-8")
        #if (verbose)
        cat("  note: converted texts from", enc, "to UTF-8.")
        documents$"_encoding" <- "UTF-8"
    }
    
    # build and return the corpus object
    tempCorpus <- list(documents=documents, 
                       metadata=metadata, 
                       settings=settings(),
                       tokens=NULL)
    class(tempCorpus) <- list("corpus", class(tempCorpus))
    return(tempCorpus)
}


#' @rdname corpus
#' @export
#' @examples
#' \donttest{# the fifth column of this csv file is the text field
#' mytexts <- textfile("http://www.kenbenoit.net/files/text_example.csv", textField=5)
#' str(mytexts)
#' mycorp <- corpus(mytexts)
#' mycorp2 <- corpus(textfile("http://www.kenbenoit.net/files/text_example.csv", textField="Title"))
#' identical(texts(mycorp), texts(mycorp2))
#' identical(docvars(mycorp), docvars(mycorp2))}
corpus.corpusSource <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    sources <- NULL
    load(x@texts, envir = environment())  # load from tempfile only into function environment
    corpus(sources$txts, docvars=sources$docv)
}

#' @rdname corpus
#' @note When \code{x} is a \link[tm]{VCorpus} object, the fixed metadata 
#'   fields from that object are imported as document-level metadata. Currently
#'   no corpus-level metadata is imported, but we will add that soon.
#' @examples 
#' #
#' ## import a tm VCorpus
#' if (require(tm)) {
#'     data(crude)    # load in a tm example VCorpus
#'     mytmCorpus <- corpus(crude)
#'     summary(mytmCorpus, showmeta=TRUE)
#' }
#' @export
corpus.VCorpus <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    texts <- sapply(x, function(x) x$content)
    
    # some mighty twisted shit here required to get a data frame from this metadata list
    metad <- as.data.frame(t(as.data.frame(sapply(x, function(x) x$meta))))
    makechar <- function(x) gsub("character\\(0\\)", NA, as.character(x))
    metad[, c(1, 3:15)] <- apply(metad[, c(1, 3:15)], 2, makechar)
    metad$datetimestamp <- t(as.data.frame((lapply(metad$datetimestamp, as.POSIXlt))))[,1]
    # give them the underscore character required
    names(metad) <- paste("_", names(metad), sep="")
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars=metad,
           source=paste("Converted from tm VCorpus \'", 
                        deparse(substitute(x)), "\'", sep=""), 
           enc=enc, ...)
}


# print a corpus object
#
# print method for corpus objects
# @param x the corpus to be printed
# @param ... further arguments passed to or from other methods
#' @export
#  see http://stackoverflow.com/questions/6517222/how-to-properly-document-a-s3-method-of-a-generic-from-a-different-package-usin
#' @method print corpus
print.corpus <- function(x, ...) {
    cat("Corpus consisting of ", ndoc(x), " document",
        ifelse(ndoc(x)>1, "s", ""), ".\n", sep="")
#         ", ",
#         ifelse(is.null(corp$tokens), "un", ""),
#         "indexed.\n", sep="")
#     cat("Settings:")
#      tempSettings <- unlist(settings(corp))
#      for (i in 1:length(tempSettings)) {
#          print(tempSettings[i])
#      }
}

#' @return \code{is.corpus} returns \code{TRUE} if the object is a corpus
#' @rdname corpus
#' @export
is.corpus <- function(x) {
    "corpus" %in% class(x)
}

#' get or set corpus metadata
#' 
#' Get or set the corpus-level metadata in a quanteda corpus object.
#' 
#' @param corp A quanteda corpus object
#' @param field Metadata field name(s).  If \code{NULL} (default), return all
#'   metadata names.
#' @return For \code{metacorpus}, a list of the metadata fields in the corpus. 
#'   If a list is not what you wanted, you can wrap the results in \link{unlist}, 
#'   but this will remove any metadata field that is set to \code{NULL}.
#'   
#'   For \code{metacorpus <-}, the corpus with the updated metadata.
#' @export
#' @examples
#' metacorpus(inaugCorpus)
#' metacorpus(inaugCorpus, "source")
#' metacorpus(inaugCorpus, "citation") <- "Presidential Speeches Online Project (2014)."
#' metacorpus(inaugCorpus, "citation")
metacorpus <- function(corp, field=NULL) {
    if (!is.corpus(corp))
        stop("Not a valid corpus object.")
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
        return(corp$metadata[field])
    } else {
        return(corp$metadata)
    }
}

# replacement function for corpus-level data
#' @param value new value of the corpus metadata field
#' @export
#' @rdname metacorpus
"metacorpus<-" <- function(corp, field, value) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    corp$metadata[field] <- value
    corp
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


#' get or set corpus texts
#' 
#' Get or replace the texts in a quanteda corpus object.
#' 
#' @param corp A quanteda corpus object
#' @return For \code{texts}, a character vector of the texts in the corpus.
#' 
#' For \code{texts <-}, the corpus with the updated texts.
#' @export
#' @examples
#' texts(inaugCorpus)[1]
#' sapply(texts(inaugCorpus), nchar)  # length in characters of the inaugual corpus texts
#'
#' ## this doesn't work yet - need to overload `[` for this replacement function
#' # texts(inaugTexts)[55] <- "GW Bush's second inaugural address, the condensed version."
texts <- function(corp) {
    temp <- documents(corp)$texts
    names(temp) <- rownames(documents(corp))
    temp
}

# replacement function for texts
# warning about no data
#' @param value character vector of the new texts
#' @rdname texts
#' @export
"texts<-" <- function(corp, value) { #}, rownames=FALSE) {
    documents(corp)$texts <- value
    # if (rownames) rownames(documents(corp)) <- names(value) 
    return(corp)
}

#' get or set document-level meta-data
#' 
#' Get or set the document-level meta-data, including reserved fields for 
#' language and corpus.
#' 
#' @param corp A quanteda corpus object
#' @param field string containing the name of the metadata field(s) to be queried or set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_encoding}, but when named in in the \code{field} argument,
#'   do \emph{not} need the underscore character.
#' @export
#' @examples
#' mycorp <- subset(inaugCorpus, Year>1990)
#' summary(mycorp, showmeta=TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta=TRUE)
metadoc <- function(corp, field=NULL) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (length(field)>1)
        stop("cannot assign multiple fields.")
    if (is.null(field)) {
        documents(corp)[, grep("^\\_", names(documents(corp))), drop=FALSE]
    } else {
        ## error if field not defined in data
        fieldname <- ifelse(substr(field, 1, 1)=="_", 
                            field, 
                            paste("_", field, sep=""))
        documents(corp)[, fieldname, drop=FALSE]
    }
}

#' @param value the new value of the new meta-data field
#' @rdname metadoc
#' @export
"metadoc<-" <- function(corp, field=NULL, value) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep="")
        if (is.null(field))
            field <- paste("_metadoc", 1:ncol(as.data.frame(value)), sep="")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(corp)[field] <- value
    corp
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
#' @examples head(docvars(inaugCorpus))
#' @export
docvars <- function(x, field=NULL) {
    docvarsIndex <- intersect(which(substr(names(documents(x)), 1, 1) != "_"),
                              which(names(documents(x)) != "texts"))
    if (length(docvarsIndex)==0)
        return(NULL)
    if (is.null(field))
        return(documents(x)[, docvarsIndex, drop=FALSE])
    #return(documents(x)[, field, drop=FALSE])
    return(documents(x)[, field, drop=TRUE])
}

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' docvars(inaugCorpus, "President") <- paste("prez", 1:ndoc(inaugCorpus), sep="")
#' head(docvars(inaugCorpus))
#' @export
"docvars<-" <- function(x, field=NULL, value) {
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
#' Extract the document names from a corpus or a document-feature matrix.  Document names are the
#' rownames of the documents data.frame in a corpus, or the rownames of the \link{dfm}
#' object for a dfm.
#' of the \link{dfm} object.
#' @param x the object with docnames
#' @export
docnames <- function(x) {
    UseMethod("docnames")
}

#' \code{docnames} queries the document names of a corpus or a dfm
#' 
#' @return \code{docnames} returns a character vector of the document names
#' @export
#' @rdname docnames
docnames.corpus <- function(x) {
    # didn't use accessor documents() because didn't want to pass
    # that large object
    rownames(x$documents)
}

#' \code{docnames <-} assigns new values to the document names of a corpus.  (Does not work
#' for dfm objects, whose document names are fixed.)
#' @param value a character vector of the same length as \code{x}
#' @return \code{docnames<-} assigns a character vector of the document names in a corpus
#' @export
#' @examples 
#' # query the document names of the inaugural speech corpus
#' docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")
#' 
#' # reassign the document names of the inaugural speech corpus
#' docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")
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
#' Returns the number of documents or features in a quanteda object.
#' @param x a corpus or dfm object
#' @param ... additional parameters
#' @return an integer (count) of the number of documents or features in the corpus or dfm
#' @export
ndoc <- function(x) {
    UseMethod("ndoc")
}

#' @rdname ndoc
#' @examples 
#' ndoc(subset(inaugCorpus, Year>1980))
#' ndoc(dfm(subset(inaugCorpus, Year>1980), verbose=FALSE))
#' @export
ndoc.corpus <- function(x) {
    nrow(x$documents)
}

#' get or set the language of corpus documents
#' 
#' Get or set the \code{_language} document-level metadata field in a corpus.
#' @param corp a corpus object
#' @param drop return as a vector if \code{TRUE}, otherwise return a \code{data.frame}
#' @details This function modifies the \code{_language} value set by
#'   \code{\link{metadoc}}.  It is a wrapper for \code{metadoc(corp, "language")}.
#' @export
language <- function(corp, drop=TRUE) {
    if ("_language" %in% names(metadoc(corp))) {
        result <- metadoc(corp, "language")
        return(result[,1, drop=drop])
    } else
        return(rep(NULL, ndoc(corp)))
}

#' @rdname language
#' @param value the new value for the language meta-data field, a string or
#'   character vector equal in length to \code{ndoc(corp)}
#' @export
"language<-" <- function(corp, value){
    metadoc(corp, "language") <- value
    # corp$documents$"_language" <- value
    corp
}

#' get the encoding of documents in a corpus
#' 
#' Get or set the \code{_encoding} document-level metadata field(s) in a corpus.
#' @param x a corpus object
#' @param drop  return as a vector if \code{TRUE}, otherwise return a \code{data.frame}
#' @details This function modifies the \code{_encoding} value set by 
#'   \code{\link{metadoc}}.  It is a wrapper for \code{metadoc(corp, "encoding")}.
#'   
#' @note This function differs from R's built-in \link{Encoding} function, which
#'   only allows the four values of "latin1", "UTF-8", "bytes", and "unknown"
#'   (and which assigns "unknown" to any text that contains only ASCII characters).
#'   Legal values for encodings must be from \link{iconvlist}.  Note that encoding
#'   does not convert or set encodings, it simply records a user declaration of a 
#'   valid encoding.  (We hope to implement checking and conversion later.)
#' @export
encoding <- function(x, drop=TRUE) {
    if ("_encoding" %in% names(metadoc(x))) {
        result <- metadoc(x, "encoding") 
        return(result[,1, drop=drop])
    } else
        return(rep(NULL, ndoc(x)))
}

#' @param value a character vector or scalar representing the new value of the encoding (see Note)
#' @rdname encoding
#' @export
"encoding<-" <- function(x, value){
    metadoc(x, "encoding") <- value
    x
}


# # Corpus sampling
# #
# # Takes a random sample of the specified size from a corpus, with or without replacement
# # 
# # @param corpus An existing corpus to be sampled
# # @param size A positive number, the number of texts to return
# # @param replace Should sampling be with replacement?
# # @param prob Not implemented
# # @export
# # @examples
# # data(inaugCorpus)
# # inaugSamp <- sample(inaugCorpus, 200, replace=TRUE)
# sample.corpus <- function(corpus, size=n, replace=FALSE, prob=NULL){
#   if(!is.null(prob)) stop("prob argument is not implemented for corpus")
#   atts <- corpus$docvars
#   sampleInds <- sample(nrow(atts), size=size, replace=replace)
#   newAtts <- atts[sampleInds,]
#   newTexts <- newAtts[[1]]
#   newAtts <- newAtts[2:length(newAtts)]
#   newCorp <- corpusCreate(newTexts, newAtts)
#   newCorp$metadata["created"] <- paste(newCorp$metadata["created"], "sampled from",
#                                        corpus$metadata["source"], collapse= " ")
#   return(newCorp)
# }


corpus.subset.inner <- function(corpus, subsetExpr=NULL, selectExpr=NULL, drop=FALSE) {
  # This is the "inner" function to be called by other functions
  # to return a subset directly, use corpus.subset
  
  # The select argument exists only for the methods for data frames and matrices. 
  # It works by first replacing column names in the selection expression with the 
  # corresponding column numbers in the data frame and then using the resulting 
  # integer vector to index the columns. This allows the use of the standard indexing 
  # conventions so that for example ranges of columns can be specified easily, 
  # or single columns can be dropped
  # as in:
  # subset(airquality, Temp > 80, select = c(Ozone, Temp))
  # subset(airquality, Day == 1, select = -Temp)
  # subset(airquality, select = Ozone:Wind)
    if (is.null(subsetExpr)) 
        rows <- TRUE
    else {
        rows <- eval(subsetExpr, documents(corpus), parent.frame())
        if (!is.logical(rows)) 
            stop("'subset' must evaluate to logical")
        rows <- rows & !is.na(rows)
    }
    
    if (is.null(selectExpr)) 
        vars <- TRUE
    else {
        nl <- as.list(seq_along(documents(corpus)))
        names(nl) <- names(documents(corpus))
        vars <- c(1, eval(selectExpr, nl, parent.frame()))
    }
    # implement subset, select, and drop
    # documents(corpus) <- documents(corpus)[rows, vars, drop=drop]
    documents(corpus) <- corpus$documents[rows, vars, drop=drop]
    return(corpus)
}


#' extract a subset of a corpus
#' 
#' Works just like the normal subset command but for corpus objects
#' 
#' @param x corpus object to be subsetted.
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param select expression, indicating the attributes to select from the corpus
#' @param ...  additional arguments affecting the summary produced
#' @return corpus object
#' @export
#' @examples
#' summary(subset(inaugCorpus, Year>1980))
#' summary(subset(inaugCorpus, Year>1930 & President=="Roosevelt", select=Year))
subset.corpus <- function(x, subset=NULL, select=NULL, ...) {
    tempcorp <- corpus.subset.inner(x, substitute(subset), substitute(select))
    return(tempcorp)
}

#' summarize a corpus or a vector of texts
#' 
#' Displays information about a corpus or vector of texts.  For a corpus, this 
#' includes attributes and metadata such as date of number of texts, creation 
#' and source.  For texts, prints to the console a desription of the texts,
#' including number of types, tokens, and sentences.
#' 
#' @param object corpus or texts to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param verbose set to \code{FALSE} to turn off printed output, for instance
#'   if you simply want to assign the output to a \code{data.frame}
#' @param showmeta for a corpus, set to \code{TRUE} to include document-level
#'   meta-data
#' @param ...  additional arguments affecting the summary produced
#' @export
#' @method summary corpus
#' @examples
#' # summarize corpus information
#' summary(inaugCorpus)
#' summary(inaugCorpus, n=10)
#' mycorpus <- corpus(ukimmigTexts, docvars=data.frame(party=names(ukimmigTexts)), enc="UTF-8")
#' summary(mycorpus, showmeta=TRUE)  # show the meta-data
#' mysummary <- summary(mycorpus, verbose=FALSE)  # (quietly) assign the results
#' mysummary$Types / mysummary$Tokens             # crude type-token ratio
#' 
summary.corpus <- function(object, n=100, verbose=TRUE, showmeta=FALSE, ...) {
    
    cat("Corpus consisting of ", ndoc(object), " document",
        ifelse(ndoc(object)>1, "s", ""), 
        ifelse(ndoc(object)<=n, "", 
               paste(", showing ", n, " document", ifelse(n>1, "s", ""), sep="")),
        ".\n", sep="")

    #print(object)
    cat("\n")
    ### Turn off describeTexts until we can speed this up
    # dtexts <- describeTexts(texts(object), verbose=FALSE)
    outputdf <- data.frame(summary(texts(object)[1:min(c(n, ndoc(object)))], 
                                         verbose=FALSE))
    if (!is.null(docvars(object)))
        outputdf <- cbind(outputdf, docvars(object)[1:min(c(n, ndoc(object))),, drop=FALSE])
    # if (detail) outputdf <- cbind(outputdf, metadoc(object))
    if (showmeta)
        outputdf[names(metadoc(object))] <- metadoc(object)[1:min(c(n, ndoc(object))),,drop=FALSE]
    if (verbose) {
        print(head(outputdf, n), row.names=FALSE)
        cat("\nSource:  ", unlist(metacorpus(object, "source")), ".\n", sep="")
        cat("Created: ",   unlist(metacorpus(object, "created")), ".\n", sep="")
        cat("Notes:   ",   unlist(metacorpus(object, "notes")), ".\n\n", sep="")
    }
    # invisibly pass the summary of the texts from describetexts()
    return(invisible(outputdf))
}

#' change the document units of a corpus
#' 
#' For a corpus, recast the documents down or up a level of aggregation.  "Down"
#' would mean going from documents to sentences, for instance.  "Up" means from 
#' sentences back to documents.  This makes it easy to reshape a corpus from a 
#' collection of documents into a collection of sentences, for instance.
#' @param corp corpus whose document units will be reshaped
#' @param to new documents units for the corpus to be recast in
#' @param ... passes additional arguments to \code{\link{segment}}

#' @export
#' @examples
#' # simple example
#' mycorpus <- corpus(c(textone="This is a sentence.  Another sentence.  Yet another.", 
#'                      textwo="Premiere phrase.  Deuxieme phrase."), 
#'                    docvars=list(country=c("UK", "USA"), year=c(1990, 2000)),
#'                    notes="This is a simple example to show how changeunits() works.")
#' language(mycorpus) <- c("english", "french")                   
#' summary(mycorpus)
#' summary(changeunits(mycorpus, to="sentences"), showmeta=TRUE)
#' 
#' # example with inaugural corpus speeches
#' mycorpus2 <- subset(inaugCorpus, Year>2004)
#' mycorpus2
#' paragCorpus <- changeunits(mycorpus2, to="paragraphs")
#' paragCorpus
#' summary(paragCorpus, 100, showmeta=TRUE)
#' ## Note that Bush 2005 is recorded as a single paragraph because that text used a single
#' ## \n to mark the end of a paragraph.
changeunits <- function(corp, to=c("sentences", "paragraphs", "documents"), ...) {
    if (!is.corpus(corp)) stop("changeunits must have a valid corpus as its first argument.")
    to <- match.arg(to)
    if (to=="documents") stop("documents not yet implemented.")
    
    # make the new corpus
    segmentedTexts <- segment(texts(corp), to, ...)
    lengthSegments <- sapply(segmentedTexts, length)
    newcorpus <- corpus(unlist(segmentedTexts))
    # repeat the docvars and existing document metadata
    docvars(newcorpus, names(docvars(corp))) <- as.data.frame(lapply(docvars(corp), rep, lengthSegments))
    docvars(newcorpus, names(metadoc(corp))) <- as.data.frame(lapply(metadoc(corp), rep, lengthSegments))
    # add original document name as metadata
    metadoc(newcorpus, "document") <- rep(names(segmentedTexts), lengthSegments)
    # give a serial number (within document) to each sentence
    sentenceid <- lapply(lengthSegments, function(n) seq(from=1, to=n))
    metadoc(newcorpus, "serialno") <- unlist(sentenceid, use.names=FALSE)
    
    # copy settings and corpus metadata
    newcorpus$settings <- corp$settings
    newcorpus$metadata <- corp$metadata

    # modify settings flag for changeunits info
    settings(newcorpus, "unitsoriginal") <- settings(newcorpus, "units")
    settings(newcorpus, "units") <- to

    newcorpus
}

rep.data.frame <- function(x, ...)
    as.data.frame(lapply(x, rep, ...))

#' @rdname corpus
#' @param c1 corpus one to be added
#' @param c2 corpus two to be added
#' @details The \code{+} operator for a corpus object will combine two corpus 
#'   objects, resolving any non-matching \code{\link{docvars}} or 
#'   \code{\link{metadoc}} fields by making them into \code{NA} values for the 
#'   corpus lacking that field.  Corpus-level meta data is concatenated, except 
#'   for \code{source} and \code{notes}, which are stamped with information 
#'   pertaining to the creation of the new joined corpus.
#'   
#'   There are some issues that need to be addressed in future revisions of 
#'   quanteda concerning the use of factors to store document variables and 
#'   meta-data.  Currently most or all of these are not recorded as factors, 
#'   because we use \code{stringsAsFactors=FALSE} in the 
#'   \code{\link{data.frame}} calls that are used to create and store the 
#'   document-level information, because the texts should always be stored as
#'   character vectors and never as factors. 
#' @export
`+.corpus` <- function(c1, c2) {
    ## deal with metadata first
    # note the source and date/time-stamp the creation
    metacorpus(c1, "source") <- paste("Combination of corpuses", deparse(substitute(c1)),
                                      "and", deparse(substitute(c2)))
    metacorpus(c1, "created") <- date()
    # concatenate the other fields if not identical already
    for (field in names(metacorpus(c2))) {
        if (field %in% c("source", "created")) next
        if (!identical(metacorpus(c1, field), metacorpus(c2, field)))
            metacorpus(c1, field) <- paste(metacorpus(c1, field), metacorpus(c2, field))
    }

    # combine the documents info, after warning if not column-conforming
    if (!setequal(names(c1$documents), names(c2$documents)))
        warning("different document-level data found, filling missing values with NAs.", noBreaks.=TRUE)
    c1$documents <- combineByName(c1$documents, c2$documents)
    
    
    # settings
    ### currently just use the c1 settings

    return(c1)
}


### from http://stackoverflow.com/questions/3402371/rbind-different-number-of-columns
### combines data frames (like rbind) but by matching column names
# columns without matches in the other data frame are still combined
# but with NA in the rows corresponding to the data frame without
# the variable
# A warning is issued if there is a type mismatch between columns of
# the same name and an attempt is made to combine the columns
combineByName <- function(A, B, ...) {
    a.names <- names(A)
    b.names <- names(B)
    all.names <- union(a.names,b.names)
    #print(paste("Number of columns:",length(all.names)))
    a.type <- NULL
    for (i in 1:ncol(A)) {
        a.type[i] <- typeof(A[,i])
    }
    b.type <- NULL
    for (i in 1:ncol(B)) {
        b.type[i] <- typeof(B[,i])
    }
    a_b.names <- names(A)[!names(A)%in%names(B)]
    b_a.names <- names(B)[!names(B)%in%names(A)]
    if (length(a_b.names)>0 | length(b_a.names)>0){
        #print("Columns in data frame A but not in data frame B:")
        #print(a_b.names)
        #print("Columns in data frame B but not in data frame A:")
        #print(b_a.names)
    } else if (a.names==b.names && a.type==b.type) {
        C <- rbind(A, B)
        return(C)
    }
    C <- list()
    for(i in 1:length(all.names)) {
        l.a <- all.names[i]%in%a.names
        pos.a <- match(all.names[i],a.names)
        typ.a <- a.type[pos.a]
        l.b <- all.names[i]%in%b.names
        pos.b <- match(all.names[i],b.names)
        typ.b <- b.type[pos.b]
        if(l.a & l.b) {
            if(typ.a==typ.b) {
                vec <- c(A[,pos.a],B[,pos.b])
            } else {
                warning(c("Type mismatch in variable named: ",all.names[i],"\n"))
                vec <- try(c(A[,pos.a],B[,pos.b]))
            }
        } else if (l.a) {
            vec <- c(A[,pos.a],rep(NA,nrow(B)))
        } else {
            vec <- c(rep(NA,nrow(A)),B[,pos.b])
        }
        C[[i]] <- vec
    }
    names(C) <- all.names
    C <- as.data.frame(C) #, stringsAsFactors=TRUE)
    return(C)
}


#' count the number of tokens
#' 
#' Return the count of tokens in a text or corpus.  "tokens" here
#' means all words, not unique words, and these are not cleaned
#' prior to counting.
#' @param x texts or corpus whose tokens will be counted
#' @param block.size how many texts to process at a time; experimentation
#' indicates that for bery large collections of texts, 200 seems fastest
#' @param verbose if \code{TRUE} print progress indicator and time elapsed
#' @return scalar count of the total tokens
#' @examples
#' ntoken(inaugTexts, verbose=FALSE)
#' ntoken(inaugCorpus, verbose=FALSE)
#' @export
ntoken <- function(x, block.size=200, verbose=TRUE) {
    UseMethod("ntoken")
}

#' @rdname ntoken
#' @export
ntoken.corpus <- function(x, block.size=200, verbose=TRUE) {
    ntoken(texts(x), block.size, verbose)
}

#' @rdname ntoken
#' @export
ntoken.character <- function(x, block.size=200, verbose=TRUE) {
    startTime <- proc.time()
    i <- 1
    totWords <- 0
    if (verbose) cat("Counting tokens (block size ", block.size, "):      ", sep="")
    while (i < length(x)) {
        pct <- i / length(x) * 100
        if (verbose) cat("\b\b\b\b\b", formatC(round(pct), width=3), " %", sep="")
        this.size <- ifelse(length(x) < i + block.size,
                            length(x) - i,
                            block.size - 1)
        totWords <- totWords + length(unlist(tokenizeOnlyCppKB(x[i:(i+this.size)])))
        i <- i + this.size + 1
    }
    if (verbose) cat("\b\b\b\b\b", formatC(100, width=3), " %", sep="")
    if (verbose) cat(", elapsed time", round((proc.time() - startTime)[3], 2), "seconds.\n")
    totWords
}

