
#' corpus source classes 
#' 
#' The \code{corpusSource} virtual class is a parent class for more specific 
#' corpus source objects.
#' 
#' @slot texts the texts that form the core of the corpus
#' @slot docvars document variables in a data.frame
#' @slot source source recorded for the corpus, based on type of source
#' @slot created a time stamp
#' @name corpusSource-class
#' @export
setClass("corpusSource", slots = c(texts = "character",
                                   docvars = "data.frame",
                                   source = "character",
                                   created = "character"))


# textsourcefile(file="myfile.xlsx", textIndex = NULL, format = NULL)
# textsourcefile(file="myfile.csv", textIndex = 1)
# textsourcefile(file="myfile.json", textIndex = 1)


#' read a text corpus source from a file
#' 
#' Read a text corpus from a source file, where the single file will consist of 
#' a set of texts in columns and document variables and document-level meta-data
#' in additional columns.  For spreadsheet-like files, the first row must be a 
#' header.
#' @param file the complete filename to be read.  Currently available file types
#'   are: \describe{ \item{\code{txt}}{plain text files} \item{\code{json}}{data
#'   in JavaScript Object Notation, consisting of the texts and additional 
#'   document-level variables and document-level meta-data.  The text key must 
#'   be identified by specifying a \code{textField} value.} 
#'   \item{\code{csv}}{comma separated value data, consisting of the texts and 
#'   additional document-level variables and document-level meta-data.  The text
#'   file must be identified by specifying a \code{textField} value.} \item{a 
#'   wildcard value}{any valid pathname with a wildcard ("glob") expression that
#'   can be expanded by the operating system.  This may consist of multiple file
#'   types.} \item{\code{xml}:}{Basic flat XML documents are supported -- those 
#'   of the kind supported by the function xmlToDataFrame function of the
#'   \pkg{XML}  package.} \item{\code{doc, docx}:}{Word files coming
#'   soon.} \item{\code{pdf}:}{Adobe Portable Document Format files, coming
#'   soon.} }
#' @param textField a variable (column) name or column number indicating where 
#'   to find the texts that form the documents for the corpus.  This must be 
#'   specified for file types \code{.csv} and \code{.json}.
#' @param docvarsfrom  used to specify that docvars should be taken from the 
#'   filenames, when the \code{textfile} inputs are filenames and the elements 
#'   of the filenames are document variables, separated by a delimiter 
#'   (\code{sep}).  This allows easy assignment of docvars from filenames such 
#'   as \code{1789-Washington.txt}, \code{1793-Washington}, etc. by \code{sep} 
#'   or from meta-data embedded in the text file header (\code{headers}).
#' @param sep separator used in filenames to delimit docvar elements if 
#'   \code{docvarsfrom="filenames"} is used
#' @param docvarnames character vector of variable names for \code{docvars}, if 
#'   \code{docvarsfrom} is specified.  If this argument is not used, default 
#'   docvar names will be used (\code{docvar1}, \code{docvar2}, ...).
#' @param ... additional arguments passed through to other functions
#' @details The constructor does not store a copy of the texts, but rather reads
#'   in the texts and associated data, and saves them to a temporary R object 
#'   whose location is specified in the \link{corpusSource-class} object.  This 
#'   prevents a complete copy of the object from cluttering the global 
#'   environment and consuming additional space.  This does mean however that 
#'   the state of the file containing the source data will not be cross-platform
#'   and may not be persistent across sessions.  So the recommended usage is to 
#'   load the data into a corpus in the same session in which \code{textfile} is
#'   called.
#' @return an object of class \link{corpusSource-class} that can be read by 
#'   \link{corpus} to construct a corpus
#' @author Kenneth Benoit and Paul Nulty
#' @export
setGeneric("textfile",
           function(file, textField, docvarsfrom=c("filenames"), sep="_", 
                    docvarnames=NULL, ...) 
               standardGeneric("textfile"),
           signature = c("file", "textField", "docvarsfrom", "sep", "docvarnames"))

# FROM THE MATRIX PACKAGE - no need to duplicate here
# setClassUnion("index", members =  c("numeric", "integer", "logical", "character"))

#' @rdname textfile
#' @export
# @importFrom streamR parseTweets
# @importFrom jsonlite  fromJSON
# (>= 0.9.10)
#' @examples 
#' # Twitter json
#' \donttest{mytf <- textfile("~/Dropbox/QUANTESS/corpora/misc/NinTANDO_Me.json")
#' summary(corpus(mytf))
#' # generic json - needs a textField specifier
#' mytf2 <- textfile("~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/sotu/sotu.json",
#'                   textField = "text")
#' summary(corpus(mytf2))
#' # text file
#' mytf3 <- textfile("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt")
#' summary(corpus(mytf3))
#' mytf4 <- textfile("~/Dropbox/QUANTESS/corpora/inaugural/*.txt")
#' summary(corpus(mytf4))
#' mytf5 <- textfile("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", 
#'                   docvarsfrom="filenames", sep="-", docvarnames=c("Year", "President"))
#' summary(corpus(mytf5))
#' # XML file
#' ## some locally working code here
#' mytf6 <- textfile("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml", 
#'                   textField = "COMMON")
#' summary(corpus(mytf6))
#' }
setMethod("textfile", 
          signature(file = "character", textField = "index", 
                    docvarsfrom="missing", sep="missing", docvarnames="missing"),
          definition = function(file, textField, ...) {
              fileType <- getFileType(file)
              if (fileType == "csv") {
                  if (length(textField) != 1)
                      stop("textField must be a single field name or column number identifying the texts.")
                  sources <- get_csv(file, textField, ...) 
              } else if (fileType == "tab") {
                  if (length(textField) != 1)
                      stop("textField must be a single field name or column number identifying the texts.")
                  sources <- get_csv(file, textField, sep = "\t", ...) 
              } else if (fileType == "json") {
                  # general json
                  sources <- get_json(file, textField, ...)
              } else if (fileType=="filemask") {
                  # only works for .csv multiple files at the moment
                  # cat("file = ", file)
                  sources <- get_csvs(file, textField, ...)
              } else if (fileType=="xml") {
                  sources <- get_xml(file, textField, ...)
              } else {
                  stop("File type ", fileType, " not yet implemented with textField.")
              }
              
              tempCorpusFilename <- tempfile()
              save(sources, file=tempCorpusFilename)
              new("corpusSource", texts=tempCorpusFilename)
          })


setMethod("textfile", 
          signature(file = "character", textField = "index", 
                    docvarsfrom="missing", sep="missing", docvarnames="missing"),
          definition = function(file, textField, ...) {
              fileType <- getFileType(file)
              if (fileType == "filemask") {
                  if (length(textField) != 1)
                      stop("textField must be a single field name or column number identifying the texts.")
                  sources <- get_csv(file, textField, ...) 
              } else if (fileType == "tab") {
                  if (length(textField) != 1)
                      stop("textField must be a single field name or column number identifying the texts.")
                  sources <- get_csv(file, textField, sep = "\t", ...) 
              } else if (fileType == "json") {
                  # general json
                  sources <- get_json(file, textField, ...)
              } else if (fileType=="csv") {
                  # only works for .csv multiple files at the moment
                  # cat("file = ", file)
                  sources <- get_csv(file, textField, ...) 
              } else if (fileType=="xml") {
                  sources <- get_xml(file, textField, ...)
              } else {
                  stop("File type ", fileType, " not yet implemented with textField.")
              }
              
              tempCorpusFilename <- tempfile()
              save(sources, file=tempCorpusFilename)
              new("corpusSource", texts=tempCorpusFilename)
          })


#' @rdname textfile
#' @export
setMethod("textfile", 
          signature(file = "character", textField = "missing",
                    docvarsfrom="missing", sep="missing", docvarnames="missing"),
          definition = function(file, textField=NULL, ...) {
              fileType <- getFileType(file)
              if (fileType == "json") {
                  # hard-wired for Twitter json only at the moment
                  sources <- get_json_tweets(file)
              } else if (fileType=="txt") {
                  sources <- list(txts=paste(readLines(file), collapse="\n"), docv=NULL)
              } else if (fileType=="filemask") {
                  sources <- get_txts(file)
              } else {
                  stop("File type ", fileType, " not supported or requires textField.")
              }
              tempCorpusFilename <- tempfile()
              save(sources, file=tempCorpusFilename)
              new("corpusSource", texts=tempCorpusFilename)
          })

#' @rdname textfile
#' @export
setMethod("textfile", 
          signature(file = "character", textField = "missing", 
                    docvarsfrom="character", sep="ANY", docvarnames="ANY"),
          definition = function(file, textField=NULL, 
                                docvarsfrom=c("headers"), sep="_", docvarnames=NULL, ...) {
              fileType <- getFileType(file)
              if (fileType=="filemask") {
                  sources <- get_txts(file, ...)
              } else {
                  stop("File type ", fileType, " not supported with these arguments.")
              }
              if (docvarsfrom == "filenames") {
                  sources$docv <- getdocvarsFromHeaders(names(sources$txts), sep=sep, docvarnames=docvarnames)
              } else {
                  warning("docvarsfrom=", docvarsfrom, " not supported.")
              }
              
              tempCorpusFilename <- tempfile()
              save(sources, file=tempCorpusFilename)
              new("corpusSource", texts=tempCorpusFilename)
          })




## New internals

# read a document from a text-only file.
get_txt <- function(f, fileType){
    switch(fileType,
            txt = paste(suppressWarnings(readLines(x)), collapse="\n"),
            doc = get_doc(f),
            pdf = get_pdf(f)
        )
}

# read a document from a structured file containing text and data
get_data <- function(f, textField='index', fileType){
    switch(fileType,
           csv = get_csv(f, textField = textField),
           json = get_json(f, textField = textField),
           xml = get_xml(f, textField = textField)
    )
}




get_doc <- function(f){
    stop('doc files not implemented yet')
}

get_pdf <- function(f){
    stop('pdf files not implemented yet')
}


## csv format
get_csv <- function(file, textField, sep=",", ...) {
    docv <- read.csv(file, stringsAsFactors=FALSE, sep=sep, ...)
    if (is.character(textField)) {
        textFieldi <- which(names(docv)==textField)
        if (length(textFieldi)==0)
            stop("column name", textField, "not found.")
        textField <- textFieldi
    }
    txts <- docv[, textField]
    docv <- docv[, -textField]
    list(txts=txts, docv=docv)
}

## INTERNALS
## specific functions for reading file types

## csv format multiple
get_csvs <- function(filemask, textField, sep=",", ...) {
    # get the pattern at the end
    pattern <- getRootFileNames(filemask)
    # get the directory name
    path <- substr(filemask, 1, nchar(filemask) - nchar(pattern))
    # get the filenames
    filenames <- list.files(path, glob2rx(pattern), full.names=TRUE)
    # read texts into a character vector
    textsvec <- c()
    docv <- NULL
    for (f in filenames) {
        thisdocv <- read.csv(f, stringsAsFactors=FALSE, sep=sep, ...)
        if (is.character(textField)) {
            textFieldi <- which(names(thisdocv)==textField)
            if (length(textFieldi)==0)
                stop("column name ", textField, " not found.")
            textField <- textFieldi
        }
        textsvec <- c(textsvec, thisdocv[, textField])
        if (is.null(docv)) {
            docv <- thisdocv[, -textField] 
        } else {
            docv <- rbind(docv, thisdocv[, -textField])
        }
    }
    list(txts=textsvec, docv=docv)
}


## csv format multiple using data.table
get_csvs2 <- function(filemask, textField, ...) {
    # get the pattern at the end
    pattern <- getRootFileNames(filemask)
    # get the directory name
    path <- substr(filemask, 1, nchar(filemask) - nchar(pattern))
    # get the filenames
    filenames <- list.files(path, glob2rx(pattern), full.names=TRUE)
    # read texts into a character vector
    textsvec <- c()
    docv <- NULL
    for (f in filenames) {
        thisdocv <- data.table::fread(f, ...)
        if (is.character(textField)) {
            textFieldi <- which(names(thisdocv)==textField)
            if (length(textFieldi)==0)
                stop("column name ", textField, " not found.")
            textField <- textFieldi
        }
        textsvec <- c(textsvec, thisdocv[, textField])
        if (is.null(docv)) {
            docv <- thisdocv[, -textField] 
        } else {
            docv <- rbind(docv, thisdocv[, -textField])
        }
    }
    list(txts=textsvec, docv=docv)
}


## Twitter json
get_json_tweets <- function(path=NULL, source="twitter", enc = "unknown", ...) {
    stopifnot(file.exists(path))
    if (!requireNamespace("streamR", quietly = TRUE))
        stop("You must have streamR installed to read Twitter json files.")
    # identifying whether it is a folder
    if (!grepl("*.json$", path)){
        # prepare list of files if it's a folder
        fls <- list.files(path, full.names=TRUE)
        fls <- fls[grepl("*.json$", fls)]
    }
    if (grepl("*.json$", path)){
        fls <- path
    }
    # read raw json data
    txt <- unlist(sapply(fls, readLines, encoding = enc))
    # crude json type check here
    if (!grepl("retweet_count", txt[1]))
        stop("Not a Twitter json formatted file.")
    
    # parsing into a data frame
    # reading tweets into a data frame
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    list(txts=results[, 1], docv=as.data.frame(results[, -1]))
}

## general json
get_json <- function(path=NULL, textField, enc = "unknown", ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    raw <- readLines(path)
    parsed <- lapply(raw, jsonlite::fromJSON, flatten=TRUE)
    df <- data.frame(matrix(unlist(parsed), nrow=length(parsed), ncol=length(parsed[[1]]), byrow=TRUE),
                     stringsAsFactors=FALSE)
    names(df) <- names(parsed[[1]])
    textFieldi <- which(names(df)==textField)
    if (length(textFieldi)==0)
        stop("column name", textField, "not found.")
    list(txts=df[, textFieldi], docv=df[, -textFieldi])
}


## flat xml format
get_xml <- function(file, textField, sep=",", ...) {
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have XML installed to read XML files.")
    docv <- XML::xmlToDataFrame(file, stringsAsFactors = FALSE)
    if (is.character(textField)) {
        textFieldi <- which(names(docv)==textField)
        if (length(textFieldi)==0)
            stop("node", textField, "not found.")
        textField <- textFieldi
    }
    txts <- docv[, textField]
    docv <- docv[, -textField]
    list(txts=txts, docv=docv)
}


getFileType <- function(filenameChar) {
    if (!substr(filenameChar, 1, 4)=="http" & grepl("[?*]", filenameChar))
        return("filemask")
    filenameParts <- strsplit(filenameChar, ".", fixed=TRUE)
    filenameExts <- sapply(filenameParts, function(x) x[length(x)])
    sapply(filenameExts, function(x) {
        if (x %in% c("xls", "xlsx"))
            return("excel")
        else if (x %in% c("csv"))
            return("csv")
        else if (x %in% c("txt"))
            return("txt")
        else if (x %in% c("doc", "docx"))
            return("word")
        else if (x %in% c("json"))
            return("json")
        else if (x %in% c("zip"))
            return("zip")
        else if (x %in% c("gz"))
            return("gz")
        else if (x %in% c("tar"))
            return("tar")
        else if (x %in% c("xml"))
            return("xml")
        else if (x %in% c("tab", "tsv"))
            return("tab")
        else return("unknown") }, USE.NAMES=FALSE)
}    


# BASED ON getTextFiles():
# load text files from disk into a vector of character vectors
#
# points to files, reads them into a character vector of the texts
# with optional names, default being filenames
# returns a named vector of complete, unedited texts
# 
# @param filemask a glob expression for text files (includes * or ?)
# @param textnames names to assign to the texts
# @param enc a value for encoding that is a legal value for \link{Encoding}
# @return character vector of texts read from disk
# @author Paul Nulty
# @export
# @examples
# \dontrun{
# getTextFiles('/home/paul/documents/libdem09.txt')
# }
get_txts <- function(filemask, textnames=NULL, ...) {
    # get the pattern at the end
    pattern <- getRootFileNames(filemask)
    # get the directory name
    path <- substr(filemask, 1, nchar(filemask) - nchar(pattern))
    # get the filenames
    filenames <- list.files(path, pattern, full.names=TRUE)
    # read texts into a character vector
    textsvec <- sapply(filenames, function(x) paste(suppressWarnings(readLines(x)), collapse="\n"))
    # name the vector with the filename by default, otherwise assign "names"
    if (!is.null(textnames)) {
        names(textsvec) <- getRootFileNames(filenames)
    } else {
        names(textsvec) <- getRootFileNames(filenames)
    }
    # apply encoding
    # Encoding(textsvec) <- enc
    
    list(txts=textsvec, docv=NULL)    
}


# Truncate absolute filepaths to root filenames
#
# This function takes an absolute filepath and returns just the 
# document name
#
# @param longFilenames Absolute filenames including a full path with directory
# @return character vector of filenames withouth directory path
# @export
# @author Paul Nulty
# @examples
# \dontrun{
# getRootFileNames('/home/paul/documents/libdem09.txt')
# }
getRootFileNames <- function(longFilenames) {
    ## function to return just the filename, path not included
    ## might need to detect .Platform$OS.type to change the delimiter
    delim <- "/"
    osName <- (Sys.info()[['sysname']] )
    
    # it is possible to use forwardslashes in Windows in R
    if ((osName=="Windows") & !('/') %in% longFilenames) { delim <- "\\\\" }
    splitFilenames <- strsplit(longFilenames, delim)
    return(sapply(splitFilenames, tail, n=1))
}


getdocvarsFromHeaders <- function(fnames, sep="_", docvarnames=NULL) {
    snames <- fnames
    snames <- gsub(".txt", "", snames)
    parts <- strsplit(snames, sep)
    if (var(sapply(parts, length)) != 0)
        stop("Filename elements are not equal in length.")
    dvars <-  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE), 
                         stringsAsFactors=FALSE)
    # assign default names in any case
    names(dvars) <- paste("docvar", 1:ncol(dvars), sep="")  
    if (!is.null(docvarnames)) {
        names(dvars)[1:length(docvarnames)] <- docvarnames
        if (length(docvarnames) != ncol(dvars)) {
            warning("Fewer docnames supplied than exist docvars - last ",
                    ncol(dvars) - length(docvarnames), " docvars were given generic names.")
        }
    }
    dvars
}
