SUPPORTED_FILETYPE_MAPPING <-        c('excel', 'excel', 'csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tsv', 'tsv')
names(SUPPORTED_FILETYPE_MAPPING) <- c('xls',   'xlsx',  'csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tab', 'tsv')

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


#' read a text corpus source from a file
#' 
#' Read a text corpus from a source file, where the single file will consist of 
#' a set of texts in columns and document variables and document-level meta-data
#' in additional columns.  For spreadsheet-like files, the first row must be a 
#' header.
#' @param file the complete filename(s) to be read.  The value can be a vector 
#'   of file names, a single file name, or a file "mask" using a "glob"-type 
#'   wildcard value.  Currently available file value types are: 
#'   \describe{ 
#'   \item{\code{txt}}{plain text files}
#'   \item{\code{json}}{data in JavaScript 
#'   Object Notation, consisting of the texts and additional document-level 
#'   variables and document-level meta-data.  The text key must be identified by
#'   specifying a \code{textField} value.}
#'   \item{\code{csv}}{comma separated 
#'   value data, consisting of the texts and additional document-level variables
#'   and document-level meta-data.  The text file must be identified by 
#'   specifying a \code{textField} value.}
#'   \item{\code{tab, tsv}}{tab-separated 
#'   value data, consisting of the texts and additional document-level variables
#'   and document-level meta-data.  The text file must be identified by 
#'   specifying a \code{textField} value.}
#'    \item{a wildcard value}{any valid 
#'   pathname with a wildcard ("glob") expression that can be expanded by the 
#'   operating system.  This may consist of multiple file types.} 
#'   \item{\code{xml}}{Basic flat XML documents are supported -- those of the 
#'   kind supported by the function xmlToDataFrame function of the \strong{XML} 
#'   package.}
#'   \item{\code{zip}}{zip archive file, containing \code{*.txt} 
#'   files either at the top level or in a single directory.
#'    This may also be a URL to a zip file.}
#'   }
#' @param ignoreMissingFiles
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
#' @param encoding vector: either the encoding of all files, or one encoding
#'   for each files
#' @param ignoreMissingFiles boolean: whether to throw an error if a missing 
#'  file is specified.
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
#' @importFrom utils download.file unzip type.convert
setGeneric("textfile",
           function(file, ignoreMissingFiles=FALSE, textField=NULL, 
                    cache = FALSE, docvarsfrom = c("filenames"), dvsep="_", 
                    docvarnames = NULL,  encoding=NULL, ...) 
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
#' mytf3 <- textfile("https://wordpress.org/plugins/about/readme.txt")
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
          signature(file = "character", ignoreMissingFiles = "ANY", textField = "ANY", 
                    cache = "ANY", docvarsfrom="ANY", dvsep="ANY", docvarnames="ANY", encoding="ANY"),
          definition = function(file, ignoreMissingFiles=FALSE, textField=NULL,
                    cache = FALSE, docvarsfrom='metadata', dvsep='_', docvarnames=NULL, encoding=NULL,
                    ...) {

              if (is.null(textField)) textField <- 1
              files <- listMatchingFiles(file, ignoreMissing=ignoreMissingFiles)

              if (is.null(encoding)) {
                  encoding <- getOption("encoding")
              }
              if (length(encoding) > 1) {
                  sources <- mapply(function(x, e) {
                      getSource(f=x, textField=textField, encoding=e, ...)
                  },
                      files, encoding,
                      SIMPLIFY=FALSE
                  )
              }
              else {
                  sources <- lapply(files, function(x) {
                      getSource(x, textField, encoding=encoding, ...)}
                  )
              }
              

              if (any(!(docvarsfrom %in% c('metadata', 'filenames'))))
                  stop("docvarsfrom must be 'metadata', 'filename', or c('metadata', 'filename')")

              docvars <- NULL
              if ('metadata' %in% docvarsfrom) {
                  docvars <- data.table::rbindlist(lapply(sources, function(x) x$docv), use.names=T, fill=T)
              }

              if ('filenames' %in% docvarsfrom) {
                  filenameDocvars <- getdocvarsFromFilenames(files, dvsep=dvsep, docvarnames=docvarnames)
                  if (!is.null(docvars)) {
                      docvars <- cbind(filenameDocvars, docvars)
                  }
                  else {
                      docvars <- filenameDocvars
                  }
              }

              returnCorpusSource(
                  list(
                       txts = unlist(lapply(sources, function(x) x$txts)),
                       docvars = data.frame(docvars)
                   ),
                  cache
              )
})



#' @importFrom stringi stri_match
#' @importFrom stringi stri_replace
listMatchingFiles <- function(x, ignoreMissing=F) {
    # There are four possible types of values for x
    #     - a simple filename
    #     - a remote URL
    #     - a glob pattern
    #     - a vector of some combination of the above

    filenames <- c()
    for (i in x) {

        scheme <- stringi::stri_match(i, regex='^([a-z][a-z+.-]*):')[, 2]
        #  First, check that this is not a URL with an unsupported scheme
        if (!(
              (scheme %in% c('http', 'https', 'ftp', 'file')) | is.na(scheme)
           )) {
            stop(paste('Unsupported URL scheme', scheme))
        }

        # If it's a file:// URL or not a URL, treat it as a local file
        if (scheme =='file' | is.na(scheme)) {
        #  here, x could be a regular filename or a glob pattern but given
        #  that regular filenames are a subset of globs, we can just treat
        #  it like a glob
        i <- stringi::stri_replace(i, replacement ='', regex='^file://')

        if (tools::file_ext(i) == 'zip' | tools::file_ext(i) == 'gz') {
            td <- mktemp(directory=T)
            utils::unzip(i, exdir = td)
            # Create a glob that matches all the files in the archive
            i <- file.path(td, '*')
        }
        globbedFiles <- Sys.glob(i)
        if ((length(globbedFiles) == 0) & !ignoreMissing) {
            stop(paste('File does not exist', i))
        }
        filenames <- c(filenames, globbedFiles)
        }

        else {
            # If this is a supported-scheme remote URL
            extension <- tools::file_ext(i)
            if (!(extension %in% names(SUPPORTED_FILETYPE_MAPPING))) {
                stop('Remote URL does not end in known extension. Please download the file manually.')
            }
            if (ignoreMissing) {
                localfile <- tryCatch({
                    localfile <- paste0(mktemp(), '.', extension) 
                    utils::download.file(i, destfile = localfile)
                    return(localfile)
                },
                error = function(e) {
                    warning(e)
                    return(NULL)
                },
                warning = function(e) {
                    warning(e)
                    return(NULL)
                }
            )}
            else {
                localfile <- paste0(mktemp(), '.', extension) 
                utils::download.file(i, destfile = localfile)
            }
            filenames <- c(filenames, localfile)
        }
    }
    filenames
}

# function common to all textfile methods to return either the cached
# textfile object link, or the textfile object itself
returnCorpusSource <- function(sources, cache = FALSE) {
    if (cache) {
        tempCorpusFilename <- mktemp()
        save(sources, file=tempCorpusFilename)
        return(new("corpusSource", cachedfile=tempCorpusFilename))
    } else
        return(new("corpusSource", texts = sources$txts, docvars = imputeDocvarsTypes(sources$docv)))
        #return(new("corpusSource", texts = sources$txts, docvars = sources$docv))
}


#' @importFrom tools file_ext
getSource <- function(f, textField, ...) {
    extension <- tools::file_ext(f)

    fileType <- tryCatch({
         SUPPORTED_FILETYPE_MAPPING[[extension]]
    }, error = function(e) {
        stop(paste('Unsupported extension', extension, 'of file', f))
    })

    switch(fileType, 
           txt = {return(get_txt(f, ...))},
           csv = {return(get_csv(f, textField, sep=',', ...))},
           tsv = {return(get_csv(f, textField, sep='\t', ...))},
           json = {return(get_json(f, textField, ...))},
           xml = {return(get_xml(f, textField, ...))}
    )
}

get_txt <- function(f, ...) {
    txt <- paste(readLines(con <- file(f, ...)), collapse="\n")
    close(con)
    list(txts=txt, docv=data.frame())
}


## csv format
get_csv <- function(path, textField, ...) {
    docs <- utils::read.csv(path, stringsAsFactors=FALSE, ...)
    if (is.character(textField)) {
        textFieldi <- which(names(docs)==textField)
        if (length(textFieldi)==0)
            stop(paste("There is no field called", textField, "in file", path))
        textField <- textFieldi
    } else if (is.numeric(textField) & (textField > ncol(docs))) {
        stop(paste0("There is no ", textField, "th field in file ", path))
    }

    txts <- docs[, textField]
    docv <- docs[, -textField, drop = FALSE]
    list(txts=txts, docv=docv)
}



#  Dispatch to get_json_object or get_json_tweets depending on whether 
#  it looks like a twitter json file
get_json <- function(path, textField, encoding, ...) {
    # encoding param is not used
    stopifnot(file.exists(path))
    tryCatch({
        return(get_json_tweets(path, ...))
    },
        error=function(e) {
            tryCatch({
                warning("Doesn't look like Tweets json file, trying general JSON")
                return(get_json_object(path, textField, ...))
            },
            error=function(e) {
                if (e == paste("There is no field called", textField, "in file", path)) {
                    stop(e)
                }
                warning("File doesn't contain a single valid JSON object, trying line-delimited json")
                return(get_json_lines(path, textField, ...))
            })
    })

}

## Twitter json
get_json_tweets <- function(path, source="twitter", ...) {
    if (!requireNamespace("streamR", quietly = TRUE))
        stop("You must have streamR installed to read Twitter json files.")
    
    # read raw json data
    txt <- readLines(path, ...)
        
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    list(txts = results[, 1], docv = as.data.frame(results[, -1, drop = FALSE]))
}

## general json
#' @importFrom data.table setDT
get_json_object <- function(path, textField, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(textField)) {
        stop('Cannot use numeric textField with json file')
    }

    docs <- jsonlite::fromJSON(path, flatten=TRUE, ...)
    docs <- data.table::setDT(docs)
    if (!(textField %in% colnames(docs))) {
        stop(paste("There is no field called", textField, "in file", path))
    }
    list(
        txts = docs[[textField]],
        docv = docs[,-textField, with=F]
    )
}

#' @importFrom data.table rbindlist
get_json_lines <- function(path, textField, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(textField)) {
        stop('Cannot use numeric textField with json file')
    }

    lines <- readLines(path)

    docs <- data.table::rbindlist(
      lapply(lines, function(x)jsonlite::fromJSON(x, flatten=TRUE, ...)),
      use.names=TRUE, fill=TRUE
    )

    if (!(textField %in% colnames(docs))) {
        stop(paste("There is no field called", textField, "in file", path))
    }
    list(
        txts = docs[[textField]],
        docv = docs[,-textField, with=F]
    )
}


## flat xml format
get_xml <- function(path, textField, encoding,...) {
    # TODO: encoding param is ignored
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have XML installed to read XML files.")

    docs <- XML::xmlToDataFrame(path, stringsAsFactors = FALSE, ...)
    if (is.numeric(textField) & (textField > ncol(docs))) {
        stop(paste0("There is no ", textField, "th field in file ", path))
    }
    if (is.character(textField)) {
        textFieldi <- which(names(docs)==textField)
        if (length(textFieldi)==0)
            stop(paste("There is no node called", textField, "in file", path))
        textField <- textFieldi
    }
    else {
        warning(paste("You should specify textField by name rather than by index, unless",
                "you're certain that your XML file's fields are always in the same order."))
    }

    txts <- docs[, textField]
    docv <- docs[, -textField, drop = FALSE]

    # Because XML::xmlToDataFrame doesn't impute column types, we have to do it
    # ourselves, to match get_csv's behaviour
    list(txts=txts, docv=docv)
}

imputeDocvarsTypes <- function(docv) {
    if (nrow(docv) == 0) return(docv)
    # Impute types of columns, just like read.table
    docv[] <- lapply(docv, function(x) type.convert(as.character(x), as.is=T))
    # And convert columns which have been made into factors into strings
    factor_cols <- vapply(docv, is.factor, FUN.VALUE=c(T))
    docv[factor_cols] <- lapply(docv[factor_cols], as.character)
    data.frame(docv)
}


#' @importFrom tools file_path_sans_ext
getdocvarsFromFilenames <- function(fnames, dvsep="_", docvarnames=NULL) {
    snames <- fnames
    snames <- tools::file_path_sans_ext(basename(snames))
    parts <- strsplit(snames, dvsep)

    if (!all(sapply(parts,function(x) identical(length(x), length(parts[[1]])))))
        stop("Filename elements are not equal in length.")

    dvars <-  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE), 
                         stringsAsFactors=FALSE)
    # assign default names in any case
    names(dvars) <- paste("docvar", 1:ncol(dvars), sep="")  
    if (!is.null(docvarnames)) {
        names(dvars)[1:length(docvarnames)] <- docvarnames
        if (length(docvarnames) != ncol(dvars)) {
            warning("Fewer docnames supplied than existing docvars - last ",
                    ncol(dvars) - length(docvarnames), " docvar",
                    ifelse((ncol(dvars) - length(docvarnames))==1, "", "s"),
                    " given generic names.")
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

mktemp <- function(prefix='tmp.', base_path=NULL, directory=F) {
    #  Create a randomly-named temporary file or directory, sort of like
    #  https://www.mktemp.org/manual.html
    if (is.null(base_path))
        base_path <- tempdir()

    alphanumeric <- c(0:9, LETTERS, letters)

    filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
    filename <- paste0(prefix, filename)
    filename <- file.path(base_path, filename)
    while (file.exists(filename) | dir.exists(filename)) {
        filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
        filename <- paste0(prefix, filename)
        filename <- file.path(base_path, filename)
    }

    if (directory) {
        dir.create(filename)
    }
    else {
        file.create(filename)
    }

    return(filename)
}
