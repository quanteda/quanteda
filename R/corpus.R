#' Constructor for corpus objects
#'
#' Creates a corpus from a character vector (of texts), or an object such as a
#' directory containing text files.  Corpus-level meta-data can be specified
#' at creation, containing (for example) citation information and notes.
#' 
#' @param texts A character vector of texts, or a filepath to a directory containing text documents.
# @param docnames Names to be assigned to the texts, defaults to the names of the 
# character vector (if any), the names of the files with the extension removed.  
# If none is supplied, it defaults to "text1", "text2", etc.
# @param docvars A data frame of attributes that is associated with each text.
# @param docvarsIn Specifies if the attributes and values are encoded in file names, directory names, or headers
# values are 'filenames', 'dirnames', or 'headers'
# @param enc Text file input encoding.  To see the system options, try \code{iconvlist()}.
# @param sep Separator for attribute values if specified in file names or directory names
# @param source Optional string specifying the source of the texts, used for referencing.
# @param notes Optional string containing notes about who created the text, warnings, To Dos, etc.
# todo examples
# @param citation Optional string specifying the citation information for this corpus.
#' @export
corpus <- function(texts, ...) {
  UseMethod("corpus")
}

# Creates a corpus from a GUI-selected set of text files
#' @rdname corpus
#' @examples 
#' \dontrun{corpus()  # pop up a file selection GUI interface
#' }
#' @export
corpus.default <- function() {
  require(tcltk2) 
  texts <- tk_choose.dir()
  return(corpus.character(texts))
}

#' Function to declare a connection to a directory (containing files)
#' 
#' Function to declare a connection to a directory, although unlike \link{file} it does not require closing.
#' If the directory does not exist, the function will return an error.
#' 
#' @param path  String describing the full path of the directory
#' @export
#' @examples 
#' \dontrun{mydir <- directory("~/Dropbox/QUANTESS/corpora/ukManRenamed")
#' corpus(mydir)} 
#' @export
directory <- function(path) {
    stopifnot(class(path) == "character")
    stopifnot(file.exists(path))
    tempPath <- path
    class(tempPath) <- list("directory", class(tempPath))
    return(tempPath)
}


#' @param docvarsfrom  Argument to specify where docvars are to be taken, from parsing the filenames (\link{filenames}) separated
#' by \code{sep} or from meta-data embedded in the text file header (\code{headers}).
#' @param docvarnames Character vector of variable names for \code{docvars}
#' @param sep Separator if \link{docvar} names are taken from the filenames.
# @warning Only files with the extension \code{.txt} are read in using the directory method.
#' @rdname corpus
#' @export
#' @examples 
#' \dontrun{
#' tempcorpus <- corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"), enc="UTF-8", source="Ken's manifesto archive")
#' summary(tempcorpus, n=10)
#' }
corpus.directory<- function(path, enc=NULL, docnames=NULL, docvarsfrom=c("filenames", "headers"), 
                            docvarnames=NULL, sep='_', 
                            source=NULL, notes=NULL, citation=NULL) {
    if (class(path)[1] != "directory") stop("path must be a directory")
    docvarsfrom <- match.arg(docvarsfrom)
    texts <- getTextDir(path)
    if (docvarsfrom=='filenames') {
        fnames <- list.files(path, full.names=TRUE)
        snames <- getRootFileNames(fnames)
        snames <- gsub(".txt", "", snames)
        parts <- strsplit(snames, sep)
        if (var(sapply(parts, length)) != 0)
            stop("Filename elements are not equal in length.")
        dvars <-  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE), 
                            stringsAsFactors=FALSE)
        if (is.null(docvarnames)) {
            names(dvars) <- paste("docvar", 1:ncol(dvars), sep="")  
        } else {
            if (ncol(dvars) != length(docvarnames)) {
                stop("The length of the parts of the filename does not equal the length of the attribute names.")
            }
        } 
        # remove the filename extension from the document names
        names(texts) <- gsub(".txt", "", names(texts))
    } else {
        stop("headers argument not yet implemented.")
    }
    
    NextMethod(texts=texts, enc=enc, docnames=docvarnames, docvars=dvars,
               source=source, notes=notes, citation=citation, fnames=fnames)
}


# Corpus constructor for a character method
# 
# Details here.
# 
#' @param texts A character vector containing the texts
#' @param docnames Names to be assigned to the texts, defaults to the names of the 
#' character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param docvars A data frame of attributes that is associated with each text.
#' @param source A string specifying the source of the texts, used for referencing.
#' @param notes A string containing notes about who created the text, warnings, To Dos, etc.
#' @rdname corpus
#' @export
#' @examples
#' corpus(inaugTexts)
#' uk2010immigCorpus <- corpus(uk2010immig, docvars=data.frame(party=names(uk2010immig)), enc="UTF-8") 
corpus.character <- function(texts, enc=NULL, docnames=NULL, docvars=NULL,
                             source=NULL, notes=NULL, citation=NULL, ...) {
    # name the texts vector
    if (!is.null(docnames)) {
        stopifnot(length(docnames)==length(texts))
        names(texts) <- docnames
    } else if (is.null(names(texts))) {
        names(texts) <- paste("text", 1:length(texts), sep="")
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
    documents <- data.frame(texts, row.names=names(texts),
                            check.rows=TRUE, stringsAsFactors=FALSE)
    # set the encoding label
    documents$"_encoding" <- enc
    # set document source if exists (from the call)
    if (exists("fnames")) {
        documents$"_source" <- fnames
    }
    
    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        stopifnot(nrow(docvars)==length(texts))
        documents <- cbind(documents, docvars)
    } 
    
    # build and return the corpus object
    tempCorpus <- list(documents=documents, 
                       metadata=metadata, 
                       settings=settingsInitialize(),
                       tokens=NULL)
    class(tempCorpus) <- list("corpus", class(tempCorpus))
    return(tempCorpus)
}



#' @export
print.corpus <- function(corp) {
    cat("Corpus consisting of ", ndoc(corp), " document",
        ifelse(ndoc(corp)>1, "s", ""),
        ", ",
        ifelse(is.null(corp$tokens), "un", ""),
        "indexed.\n", sep="")
}


#' Corpus-level metadata
#' 
#' Get or set the corpus-level metadata in a quanteda corpus object.
#' 
#' @param corp A quanteda corpus object
#' @param fields Metadata field names.  If NULL (default), return all metadata names.
#' @return For \code{metacorpus}, a list of the metadata fields in the corpus.
#' 
#' For \code{metacorpus <-}, the corpus with the updated metadata.
#' @export
#' @examples
#' metacorpus(inaugTexts)
#' metacorpus(inaugTexts, "source")
#' metacorpus(inaugTexts, "citation") <- "Presidential Speeches Online Project (2014)."
metacorpus <- function(corp, fields=NULL) {
    if (!is.null(fields)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
        return(corp$metadata[fields])
    } else {
        return(corp$metadata)
    }
}

# replacement function for corpus-level data
#' @export
#' @rdname metacorpus
"metacorpus<-" <- function(corp, value, fields) {
    if (!is.null(fields)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    corp$metadata[fields] <- value
    corp
}


# internal accessor for documents object
#' @export
documents <- function(corp) {
    corp$documents
}
# internal replacement function for documents
#' @export
"documents<-" <- function(corp, value) {
    corp$documents <- value
    corp
}


#' Corpus texts
#' 
#' Get or replace the texts in a quanteda corpus object.
#' 
#' @param corp A quanteda corpus object
#' @return For \code{texts}, a character vector of the texts in the corpus.
#' For \code{texts <-}, the corpus with the updated texts.
#' @export
#' @examples
#' texts(inaugCorpus)[1]
#' texts(inaugTexts)[55] <- "GW Bush's second inaugural address, the condensed version."
texts <- function(corp) {
    temp <- documents(corp)$texts
    names(temp) <- rownames(documents(corp))
    temp
}

# replacement function for texts
# warning about no data
#' @param rownames If TRUE, overwrite the names of the documents with names from assigned object.
#' @rdname texts
#' @export
"texts<-" <- function(corp, value, rownames=FALSE) {
    documents(corp)$texts <- value
    if (rownames) rownames(documents(corp)) <- names(value) 
    return(corp)
}

# accessor for document-level metadata
#' @export
metadoc <- function(corp, field=NULL) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        documents(corp)[, grep("^\\_", names(documents(corp))), drop=FALSE]
    } else {
        ## error if field not defined in data
        documents(corp)[, paste("_", field, sep=""), drop=FALSE]
    }
}

# replacement function for document-level metadata
#' @export
"metadoc<-" <- function(corp, value, field) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    field <- paste("_", field, sep="")
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
#' @export
"metadoc<-[" <- function(corp, value, field) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    field <- paste("_", field, sep="")
    documents(corp)[field] <- value
    corp
}



# accessor for document variables
#' @export
docvars <- function(corp) {
    docvarsIndex <- intersect(which(substr(names(documents(corp)), 1, 1) != "_"),
                              which(names(documents(corp)) != "texts"))
    if (length(docvarsIndex)==0) {
        return(NULL)
    } else {
        return(documents(corp)[, docvarsIndex, drop=FALSE])
    }
}

# replacement function for document variables
#' @export
"docvars<-" <- function(corp, value, field) {
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    documents(corp)[field] <- value
    corp
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

# accessor for docnames
#' @export
docnames <- function(corp) {
    # didn't use accessor documents() because didn't want to pass
    # that large object
    rownames(corp$documents)
}

# replacement function for docnames
#' @export
"docnames<-" <- function(corp, value) {
  rownames(corp$documents) <- value
  return(corp)
}


#' get the number of documents
#' 
#' Returns the number of documents.
#' @export
ndoc <- function(x) {
    UseMethod("ndoc")
}


#' get the number of documents
#' 
#' Returns the number of documents in a corpus objects
#' @param corp a quanteda corpus object
#' @rdname ndoc
#' @value an integer (count) of the number of documents in the corpus
#' @examples ndoc(inaugCorpus)
#' @export
ndoc.corpus <- function(corp) {
    nrow(corp$documents)
}

# accessor for language
#' @export
language <- function(corp) {
    if ("_language" %in% names(metadoc(corp)))
        metadoc(corp, "language") 
    else
        rep(NULL, ndoc(corp))
}

# replacement function for language
#' @export
"language<-" <- function(corp, value){
    metadoc(corp, "language") <- value
    # corp$documents$"_language" <- value
    corp
}

# accessor for encoding
#' @export
encoding <- function(corp) {
    if ("_encoding" %in% names(metadoc(corp)))
        metadoc(corp, "encoding") 
    else
        rep(NULL, ndoc(corp))
}

# replacement function for encoding
#' @export
"encoding<-" <- function(corp, value){
    metadoc(corp, "encoding") <- value
    corp
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
    documents(corpus) <- documents(corpus)[rows, vars, drop=drop]
    return(corpus)
}


#' extract a subset of a corpus
#' 
#' Works just like the normal subset command but for corpus objects
#' 
#' @param corpus corpus object to be subsetted.
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param select expression, indicating the attributes to select from the corpus
#' @return corpus object
#' @export
#' @examples
#' \dontrun{
#' data(inaugCorpus)
#' inaugCorpus <- subset(inaugCorpus, year==2010)
#' summary(iebudgets2010)
#' iebudgetsCarter <- subset(iebudgets, speaker="Carter", select=c(speaker, year))
#' summary(iebudgetsLenihan)
#' }
subset.corpus <- function(corpus, subset=NULL, select=NULL) {
    tempcorp <- corpus.subset.inner(corpus, substitute(subset), substitute(select))
    return(tempcorp)
}

#' Corpus summary
#'
#' Displays information about a corpus object, including attributes and 
#' metadata such as date of number of texts, creation and source.
#' 
#' @param corp corpus to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param verbose FALSE to turn off printed output
#' @param meta TRUE to include document-level meta-data
#' @export
#' @examples
#' summary(inaugCorpus)
#' summary(inaugCorpus, n=10, printdocnames=FALSE)
#' mycorpus <- corpus(uk2010immig, docvars=data.frame(party=names(uk2010immig)), enc="UTF-8")
#' summary(mycorpus, meta=TRUE)  # show the meta-data
#' mysummary <- summary(mycorpus, verbose=FALSE)  # (quietly) assign the results
#' mysummary$Types / mysummary$Tokens             # crude type-token ratio
summary.corpus <- function(corp, n=100, verbose=TRUE, meta=FALSE) {
    print(corp)
    cat("\n")
    ### Turn off describeTexts until we can speed this up
    # dtexts <- describeTexts(texts(corp), verbose=FALSE)
    outputdf <- data.frame(describeTexts(texts(corp), verbose=FALSE),
                           docvars(corp))
    if (meta)
        outputdf[names(metadoc(corp))] <- metadoc(corp)
    if (verbose) {
        print(head(outputdf, n), row.names=FALSE)
        cat("\nSource:  ", metacorpus(corp, "source"), ".\n", sep="")
        cat("Created: ",   metacorpus(corp, "created"), ".\n", sep="")
        cat("Notes:   ",   metacorpus(corp, "notes"), ".\n\n", sep="")
    }
    # invisibly pass the summary of the texts from describetexts()
    return(invisible(outputdf))
}

