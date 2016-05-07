#' corpus source classes
#' 
#' The \code{corpusSource} virtual class is a parent class for more specific 
#' corpus source objects.
#' 
#' @slot texts the texts that form the core of the corpus
#' @slot docvars document variables in a data.frame
#' @slot source source recorded for the corpus, based on type of source
#' @slot created a time stamp
#' @slot cachedfile if read to a temporary file, a string containing the
#'   location of the temporary file
#' @name corpusSource-class
#' @export
setClass("corpusSource", slots = c(texts = "character",
                                   docvars = "data.frame",
                                   source = "character",
                                   created = "character",
                                   cachedfile = "character"),
         prototype = list(cachedfile = ""))


#' @rdname corpusSource-class
#' @param object corpusSource object to be printed
#' @export
setMethod("show",
          signature(object = "corpusSource"), function(object) {
              if (object@cachedfile != "") {
                  cat("corpusSource object with data cached in", object@cachedfile, "\n")
              } else {
                  cat("corpusSource object consisting of ", length(texts(object)), 
                      " document", ifelse(length(texts(object)) == 1, "", "s"), " and ", 
                      ncol(docvars(object)), " docvar", ifelse(ncol(docvars(object)) == 1, "", "s"), ".\n", sep="")
              }
          })


# textsourcefile(file="myfile.xlsx", textIndex = NULL, format = NULL)
# textsourcefile(file="myfile.csv", textIndex = 1)
# textsourcefile(file="myfile.json", textIndex = 1)


#' read a text corpus source from a file
#' 
#' Read a text corpus from a source file, where the single file will consist of 
#' a set of texts in columns and document variables and document-level meta-data
#' in additional columns.  For spreadsheet-like files, the first row must be a 
#' header.
#' @param file the complete filename(s) to be read.  The value can be a vector 
#'   of file names, a single file name, or a file "mask" using a "glob"-type 
#'   wildcard value.  Currently available file value types are: \describe{ 
#'   \item{\code{txt}}{plain text files} \item{\code{json}}{data in JavaScript 
#'   Object Notation, consisting of the texts and additional document-level 
#'   variables and document-level meta-data.  The text key must be identified by
#'   specifying a \code{textField} value.} \item{\code{csv}}{comma separated 
#'   value data, consisting of the texts and additional document-level variables
#'   and document-level meta-data.  The text file must be identified by 
#'   specifying a \code{textField} value.} \item{\code{tab, tsv}}{tab-separated 
#'   value data, consisting of the texts and additional document-level variables
#'   and document-level meta-data.  The text file must be identified by 
#'   specifying a \code{textField} value.} \item{a wildcard value}{any valid 
#'   pathname with a wildcard ("glob") expression that can be expanded by the 
#'   operating system.  This may consist of multiple file types.} 
#'   \item{\code{xml}}{Basic flat XML documents are supported -- those of the 
#'   kind supported by the function xmlToDataFrame function of the \strong{XML} 
#'   package.} \item{\code{zip}}{zip archive file, containing \code{*.txt} 
#'   files.  This may be a URL to a zip file.} }
#' @param textField a variable (column) name or column number indicating where 
#'   to find the texts that form the documents for the corpus.  This must be 
#'   specified for file types \code{.csv} and \code{.json}.
#' @param docvarsfrom  used to specify that docvars should be taken from the 
#'   filenames, when the \code{textfile} inputs are filenames and the elements 
#'   of the filenames are document variables, separated by a delimiter 
#'   (\code{dvsep}).  This allows easy assignment of docvars from filenames such
#'   as \code{1789-Washington.txt}, \code{1793-Washington}, etc. by \code{dvsep}
#'   or from meta-data embedded in the text file header (\code{headers}).
#' @param dvsep separator used in filenames to delimit docvar elements if 
#'   \code{docvarsfrom="filenames"} is used
#' @param docvarnames character vector of variable names for \code{docvars}, if 
#'   \code{docvarsfrom} is specified.  If this argument is not used, default 
#'   docvar names will be used (\code{docvar1}, \code{docvar2}, ...).
#' @param cache If \code{TRUE}, write the object to a temporary file and store 
#'   the temporary filename in the \link{corpusSource-class} object definition. 
#'   If \code{FALSE}, return the data in the object. Caching the file provides a
#'   way to read in very large quantities of textual data without storing two 
#'   copies in memory: one as a \link{corpusSource-class} object and the second 
#'   as a \link{corpus} class object.  It also provides a way to try different 
#'   settings of encoding conversion when creating a corpus from a 
#'   \link{corpusSource-class} object, without having to load in all of the 
#'   source data again.
#' @param ... additional arguments passed through to low-level file reading 
#'   function, such as \code{\link{file}}, \code{\link{read.csv}}, etc.  Useful 
#'   for specifying an input encoding option, which is specified in the same was
#'   as it would be give to \code{\link{iconv}}.  See the Encoding section of 
#'   \link{file} for details.  Also useful for passing arguments through to
#'   \code{\link{read.csv}}, for instance `quote = ""`, if quotes are causing
#'   problems within comma-delimited fields.
#' @details The constructor does not store a copy of the texts, but rather reads
#'   in the texts and associated data, and saves them to a temporary disk file 
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
#' @importFrom stats var
#' @importFrom utils download.file unzip
setGeneric("textfile",
           function(file, textField, 
                    cache = FALSE, docvarsfrom = c("filenames"), dvsep="_", 
                    docvarnames = NULL,  ...) 
               standardGeneric("textfile"))
#signature = c("file", "textField", "encodingFrom", "encodingTo", "docvarsfrom", 
#              "dvsep", "docvarnames", "cache", "encodingFrom", "encodingTo"))

# FROM THE MATRIX PACKAGE - no need to duplicate here
# setClassUnion("index", members =  c("numeric", "integer", "logical", "character"))

#' @rdname textfile
#' @export
#' @examples 
#' \dontrun{# Twitter json
#' mytf1 <- textfile("http://www.kenbenoit.net/files/tweets.json")
#' summary(corpus(mytf1), 5)
#' # generic json - needs a textField specifier
#' mytf2 <- textfile("http://www.kenbenoit.net/files/sotu.json",
#'                   textField = "text")
#' summary(corpus(mytf2))
#' # text file
#' mytf3 <- textfile(unzip(system.file("extdata", "pg2701.txt.zip", package = "quanteda")))
#' summary(corpus(mytf3))
#' # XML data
#' mytf6 <- textfile("http://www.kenbenoit.net/files/plant_catalog.xml", 
#'                   textField = "COMMON")
#' summary(corpus(mytf6))
#' # csv file
#' write.csv(data.frame(inaugSpeech = texts(inaugCorpus), docvars(inaugCorpus)), 
#'           file = "/tmp/inaugTexts.csv", row.names = FALSE)
#' mytf7 <- textfile("/tmp/inaugTexts.csv", textField = "inaugSpeech")
#' summary(corpus(mytf7))
#' 
#' # vector of full filenames for a recursive structure
#' textfile(list.files(path = "~/Desktop/texts", pattern = "\\.txt$", 
#'                     full.names = TRUE, recursive = TRUE))
#' }
setMethod("textfile", 
          signature(file = "character", textField = "index", 
                    cache = "ANY", 
                    docvarsfrom="missing", dvsep="missing", docvarnames="missing"),
          definition = function(file, textField, cache = FALSE, ...) {
              #               if ((!(addedArgs <- list(...)) %in% names(formals(file))))
              #                   warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
              if (length(textField) != 1)
                  stop("textField must be a single field name or column number identifying the texts.")
              fileType <- getFileType(file)
              # print('1')
              if (fileType == 'filemask'){
                  sources <- get_datas(file, textField, ...)
              } else {
                  sources <- get_data(file, textField, fileType, ...)
              }
              returnCorpusSource(sources, cache)
          })


#' @rdname textfile
#' @export
setMethod("textfile", 
          signature(file = "character", textField = "missing",
                    cache = "ANY",
                    docvarsfrom="missing", dvsep="missing", docvarnames="missing"),
          definition = function(file, cache = FALSE, ...) {
              #               if (length(addedArgs <- list(...)))
              #                   warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
              
              fileType <- getFileType(file)
              if (fileType=="filemask" | fileType=="vector") {
                  sources <- get_docs(file, ...)
              } else {
                  sources <- get_doc(file, ...)
              }
              returnCorpusSource(sources, cache)
          })

#' @rdname textfile
#' @export
setMethod("textfile", 
          signature(file = "character", textField = "missing", 
                    cache = "ANY",
                    docvarsfrom="character", dvsep="ANY", docvarnames="ANY"),
          definition = function(file, textField=NULL, cache = FALSE, 
                                docvarsfrom=c("headers"), dvsep="_", docvarnames=NULL, ...) {
              #               if (length(addedArgs <- list(...)))
              #                   warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
              fileType <- getFileType(file)
              if (fileType=="filemask") {
                  sources <- get_docs(file, ...)
              } else {
                  stop("File type ", fileType, " not supported with these arguments.")
              }
              if (docvarsfrom == "filenames") {
                  sources$docv <- getdocvarsFromHeaders(names(sources$txts), dvsep=dvsep, docvarnames=docvarnames)
              } else {
                  warning("docvarsfrom=", docvarsfrom, " not supported.")
              }
              returnCorpusSource(sources, cache)
          })

## New internals

# function common to all textfile methods to return either the cached
# textfile object link, or the textfile object itself
returnCorpusSource <- function(sources, cache = FALSE) {
    if (cache) {
        tempCorpusFilename <- tempfile()
        save(sources, file=tempCorpusFilename)
        return(new("corpusSource", cachedfile=tempCorpusFilename))
    } else
        return(new("corpusSource", texts = sources$txts, docvars = sources$docv))
}


# read a document from a text-only file.
get_doc <- function(f, ...) {
    txts <- c()
    fileType <- getFileType(f)
    # cat("fileType = ", fileType, "\n")
    switch(fileType,
           txt =  { 
               txt <- readLines(con <- file(f, ...), warn = FALSE)
               close(con)
               
               # convert to UTF-8 if an input encoding was specified and if
               # the native.enc is not already UTF-8
#                if ("encoding" %in% names(args <- list(...)) & !(grepl("UTF-8", Sys.getlocale("LC_CTYPE")))) {
#                    iconv(txt, from = args$encoding, to = "UTF-8")
#                }
               
               
               result <- list(txts = paste(txt, collapse="\n"), docv = data.frame())
               return(result)
           },
           doc =  { return(list(txts = get_word(f), docv = data.frame())) },
           json = { return(get_json_tweets(f, ...)) },
           zip = { return(get_zipfile(f)) },
           pdf =  { return(list(txts = get_pdf(f), docv = data.frame())) }
    )
    stop("unrecognized fileType:", fileType)
}

get_docs <- function(filemask, ...) {
    if (length(filemask) == 1) {
        # get the pattern at the end, as a regex
        pattern <- utils::glob2rx(basename(filemask))
        # get the directory name
        path <- dirname(filemask)
        # get the filenames
        filenames <- list.files(path, pattern, full.names=TRUE)
    } else {
        filenames <- filemask
    }
    
    # read texts from call to get_doc, discarding any docv
    if ("encoding" %in% names(args <- list(...))) {
        encodingFrom <- args$encoding
    } else {
        encodingFrom <- getOption("encoding")
    }
    
    #cat("arg = ", args, "\n\n")
    
    if (!is.null(encodingFrom)) {
        if (length(encodingFrom) > 1) {
            if (length(filenames) != length(encodingFrom))
                stop("length of encodingFrom (", length(encodingFrom), ") different from length of filenames (", length(filenames), ")")    
        } else
            encodingFrom <- rep(encodingFrom, length(filenames))
    }
    # loop through filenames and load each one
    textsvec <- c()
    for (i in 1:length(filenames))
        # cat("encoding = ", encodingFrom[i], "\n")
        textsvec[i] <- get_doc(filenames[i], encoding = encodingFrom[i])$txts
    
    # name the vector with the filename by default
    names(textsvec) <- basename(filenames)
    
    list(txts = textsvec, docv = data.frame())    
}

get_zipfile <- function(f, ...) {
    td <- tempdir()
    if (substr(f, 1, 4) == "http")
        utils::download.file(f, destfile = (flocal <- paste0(td, "/temp.zip", quiet = TRUE)))
    utils::unzip(flocal, exdir = td)
    # cat("file:", paste0(td, "*.txt"), "\n")
    get_docs(paste0(td, "/*.txt"))
}

# read a document from a structured file containing text and data
get_data <- function(f, textField, sep = ",", ...){
    src <- list()
    # print('fileType')
    fileType <- getFileType(f)
    switch(fileType,
           csv = {src <- get_csv(f, textField, ...)},
           tab = {src <- get_csv(f, textField, sep = "\t", ...)},
           tsv = {src <- get_csv(f, textField, sep = "\t", ...)},
           json = {src <- get_json(f, textField, ...)},
           xml = {src <- get_xml(f, textField, ...)}
    )
    # print(names(src))
    return(src)
}

# read a document from a structured file containing text and data
get_datas <- function(filemask, textField='index', fileType, ...){
    # get the pattern at the end
    pattern <- basename(filemask)
    # get the directory name
    path <- dirname(filemask)
    # get the filenames
    filenames <- list.files(path, utils::glob2rx(pattern), full.names=TRUE)
    # read texts into a character vector
    textsvec <- c()
    docv <- data.frame()
    for (f in filenames) {
        src <- get_data(f,  textField, ...)
        textsvec <- c(textsvec, src$txts)
	docv <- tryCatch({
		rbind(docv, src$docv)
	},
		error = function(e) {
			stop('Data files do not have identical columns or variables')
	}
	)
    }
    list(txts=textsvec, docv=docv)
    # return(src)
}

get_word <- function(f){
    stop('doc files not implemented yet')
}

get_pdf <- function(f){
    stop('pdf files not implemented yet')
}

## csv format
get_csv <- function(file, textField, sep=",", ...) {
    docv <- utils::read.csv(file, stringsAsFactors=FALSE, sep=sep, ...)
    if (is.character(textField)) {
        textFieldi <- which(names(docv)==textField)
        if (length(textFieldi)==0)
            stop("column name ", textField, " not found.")
        textField <- textFieldi
    }
    txts <- docv[, textField]
    docv <- docv[, -textField, drop = FALSE]
    list(txts=txts, docv=docv)
}



## Twitter json
get_json_tweets <- function(path=NULL, source="twitter", ...) {
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
    txt <- unlist(sapply(fls, readLines, ...))
    # crude json type check here
    if (!grepl("retweet_count", txt[1]))
        stop("Not a Twitter json formatted file.")
    
    # parsing into a data frame
    # reading tweets into a data frame
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    list(txts = results[, 1], docv = as.data.frame(results[, -1, drop = FALSE]))
}

## general json
get_json <- function(path, textField, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    # raw <- readLines(path)
    #parsed <- lapply(path, jsonlite::fromJSON, flatten=TRUE)
    df <- jsonlite::fromJSON(path, flatten=TRUE, ...)
    #     df <- data.frame(matrix(unlist(parsed), nrow=length(parsed), ncol=length(parsed[[1]]), byrow=TRUE),
    #                      stringsAsFactors=FALSE)
    #     names(df) <- names(parsed[[1]])
    textFieldi <- which(names(df)==textField)
    if (length(textFieldi)==0)
        stop("column name", textField, "not found.")
    list(txts=df[, textFieldi], docv=df[, -textFieldi, drop = FALSE])
}


## flat xml format
get_xml <- function(file, textField, sep=",", ...) {
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have XML installed to read XML files.")
    docv <- XML::xmlToDataFrame(file, stringsAsFactors = FALSE, ...)
    if (is.character(textField)) {
        textFieldi <- which(names(docv)==textField)
        if (length(textFieldi)==0)
            stop("node", textField, "not found.")
        textField <- textFieldi
    }
    txts <- docv[, textField]
    docv <- docv[, -textField, drop = FALSE]
    list(txts=txts, docv=docv)
}


getFileType <- function(filenameChar) {
    if (length(filenameChar) > 1)
        return("vector")
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


getdocvarsFromHeaders <- function(fnames, dvsep="_", docvarnames=NULL) {
    snames <- fnames
    snames <- gsub(".txt", "", snames)
    parts <- strsplit(snames, dvsep)
    if (stats::var(sapply(parts, length)) != 0)
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

#' @rdname texts
#' @export
texts.corpusSource <- function(x, groups = NULL, ...) {
    sources <- NULL
    if (!is.null(groups))
        stop("groups argument not supported for texts() on a corpusSource object")
    if (x@cachedfile == "") {
        return(x@texts)
    } else {
        # load from tempfile only into function environment
        load(x@cachedfile, envir = environment())
        return(sources$txts)
    }
}

#' @rdname docvars
#' @export
docvars.corpusSource <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a corpusSource object", noBreaks. = TRUE)
    x@docvars
}

