SUPPORTED_FILETYPE_MAPPING <-        c('csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tsv', 'tsv')
names(SUPPORTED_FILETYPE_MAPPING) <- c('csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tab', 'tsv')

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
#' Read a text corpus from one or more source files. The texts of the corpus
#' come from (some part of) the content of the files, and the document-level
#' metadata (docvars) come from either the file contents or filenames.
#' @param file the complete filename(s) to be read. This is designed to 
#'   automagically handle a number of common scenarios, so the value can be a
#    single filename, a vector of file names a remote URL, or a file "mask" using a 
#'   "glob"-type'  wildcard value.  Currently available filetypes are: 
#'   \describe{
#'   \item{\code{txt}}{plain text files:
#'   So-called structured text files, which describe both texts and metadata:
#'   For all structured text filetypes, the column, field, or node 
#'   which contains the the text must be specified with the \code{textField}
#'   parameter, and all other fields are treated as docvars.}
#'   \item{\code{json}}{data in some form of JavaScript 
#'   Object Notation, consisting of the texts and optionally additional docvars.
#'   The supported formats are:
#'   \itemize{
#'   \item a single JSON object per file
#'   \item line-delimited JSON, with one object per line
#'   \item line-delimited JSON, of the format produced from a Twitter stream.
#'   This type of file has special handling which simplifies the Twitter format
#'   into docvars.  The correct format for each JSON file is automatically detected.}}
#'   \item{\code{csv,tab,tsv}}{comma- or tab-separated values}
#'   \item{\code{xml}}{Basic flat XML documents are supported -- those of the 
#'   kind supported by the function xmlToDataFrame function of the \strong{XML} 
#'   package.}
#'   \code{file} can also not be a path to a single local file, such as
#'    \item{a wildcard value}{any valid 
#'   pathname with a wildcard ("glob") expression that can be expanded by the 
#'   operating system.  This may consist of multiple file types.} 
#'   \item{a URL to a remote}{which is downloaded then loaded} 
#'   \item{\code{zip,tar,tar.gz,tar.bz}}{archive file, which is unzipped. The 
#'   contained files must be either at the top level or in a single directory.
#'   Archives, remote URLs and glob patterns can resolve to any of the other 
#'   filetypes, so you could have, for example, a remote URL to a zip file which
#'   contained Twitter JSON files.}
#'   }
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
#'   source data again
#' @param encoding vector: either the encoding of all files, or one encoding
#'   for each files
#' @param ignoreMissingFiles if \code{FALSE}, then if the file
#'   argument doesn't resolve to an existing file, then an error will be thrown.
#'   Note that this can happen in a number of ways, including passing a path 
#'   to a file that does not exist, to an empty archive file, or to a glob 
#'   pattern that matches no files.
#' @param ... additional arguments passed through to low-level file reading 
#'   function, such as \code{\link{file}}, \code{\link{read.csv}}, etc.  Useful 
#'   for specifying an input encoding option, which is specified in the same was
#'   as it would be give to \code{\link{iconv}}.  See the Encoding section of 
#'   \link{file} for details.  Also useful for passing arguments through to
#'   \code{\link{read.csv}}, for instance `quote = ""`, if quotes are causing
#'   problems within comma-delimited fields.
#' @details If \code{cache = TRUE}, the constructor does not store a copy of 
#'   the texts, but rather reads
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
#' @author Adam Obeng, Kenneth Benoit, and Paul Nulty
#' @export
#' @importFrom utils unzip type.convert
#' @importFrom httr GET write_disk

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
                  if (length(encoding) != length(files)) {
                    stop('encoding parameter must be length 1, or as long as the number of files')
                  }
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



downloadRemote <- function (i, ignoreMissing) {
    # First, check that this is not a URL with an unsupported scheme
    scheme <- stringi::stri_match(i, regex='^([a-z][a-z+.-]*):')[, 2]
    if (!(scheme %in% c('http', 'https', 'ftp'))) {
        stop(paste('Unsupported URL scheme', scheme))
    }

    # If this is a supported-scheme remote URL
    extension <- tools::file_ext(i)
    if (!(extension %in% names(SUPPORTED_FILETYPE_MAPPING))) {
        stop('Remote URL does not end in known extension. Please download the file manually.')
    }
    localfile <- file.path(mktemp(directory=T), basename(i))
    r <- httr::GET(i, httr::write_disk(localfile))
    if (ignoreMissing) {
        httr::warn_for_status(r)
        if (httr::http_error(r)) {
            return(NULL)
        }
    }
    else {
        httr::stop_for_status(r)
    }
    localfile
}

listMatchingFiles <- function(x, ignoreMissing=F, lastRound=F) {
    #  The implementation of listMatchingFiles and listMatchingFile might seem
    #  very complex, but it was arrived at after a lot of toil. The main design
    #  decision made here is that the user should be able to pass many
    #  different types of string to listMatchingFiles and get a consistent result:
    #  a list of local filenames. (One additional wrinkle is that there are two
    #  functions, listMatchingFiles and listMatchingFile. This is to ensure that
    #  listMatchingFile is only ever called with a length 1 argument, even though
    #  it can return multiple filenames. For the purposes of this explanation, 
    #  this distinction is elided).
    #  There are four possible types of values for x
    #     - a simple filename
    #     - a remote URL
    #     - a glob pattern
    #     - a vector of some combination of the above
    #  listMatchingFiles has a recursive design, because  some of these 
    #  arguments can resolve to arguments which need further processing: e.g.
    #  a remote URL could resolve to a zip file which needs to be extracted.
    #  The termination condition for the recursion is when the argument passed
    #  is a local filepath which refers to a single file and needs no further
    #  processing, e.g. something like '/path/to/text.tsv'. However, it is not
    #  possible to determine if a given argument is a path to a single file 
    #  or a glob pattern which matches multiple files, without actually trying
    #  the match. This matters because if it's the result of a globbing expression,
    #  then it could potentially need further processing, but if it's not, it the recursion
    #  needs to end. We can't know beforehand because the rules for globbing are 
    #  implementation-dependent (systems might treat '/path/to/file\*.tsv' as
    #  either a filename or a path depending on  whether they support escaping
    #  of glob wildcards. We could have tested the return value from Sys.glob
    #  to see whether the system treats a given string as a glob pattern or a 
    #  simple filename. Unfortunately, Sys.glob() will return character(0)
    #  for either a glob pattern which matches no files, or a non-glob filename
    #  for a file that doesn't exist, so that doesn't work either.
    #  We also can't test whether a pattern is a regular file by looking at the
    #  extension, because '/path/to/*.zip' is a glob expression with a 'zip'
    #  extension.

    if (!(ignoreMissing || (length(x) > 0))) {
       stop("File does not exist.")
    }

    matchingFiles <- unlist(
        lapply(x, function (x) listMatchingFile(
            x,
            ignoreMissing=ignoreMissing,
            lastRound=lastRound)
        )
    )

    if (is.null(matchingFiles)) return(character(0))

    matchingFiles
}

extractArchive <- function(i, ignoreMissing) {
    if (!(ignoreMissing || file.exists(i)))
        stop(paste("File", i, "does not exist."))

    td <- mktemp(directory=T)
    if (tools::file_ext(i) == 'zip')
        utils::unzip(i, exdir = td)
    else if ( tools::file_ext(i) == 'gz' ||
        tools::file_ext(i) == 'tar' ||
        tools::file_ext(i) == 'bz' )
        utils::untar(i, exdir = td)

    # Create a glob that matches all the files in the archive
    file.path(td, '*')
}

#' @importFrom stringi stri_match
#' @importFrom stringi stri_replace
listMatchingFile <- function(x, ignoreMissing, verbose=F, lastRound) {

    filenames <- c()
    #  Remove 'file' scheme
    i <- stringi::stri_replace(x, replacement ='', regex='^file://')

    scheme <- stringi::stri_match(i, regex='^([a-z][a-z+.-]*):')[, 2]
    
    # If not a URL (or a file:// URL) , treat it as a local file
    if (!is.na(scheme)) {
        if (verbose) message('Remote file')
        #  If there is a non-'file' scheme, treat it as remote
        localfile <- downloadRemote(i, ignoreMissing=ignoreMissing)
        return(listMatchingFiles(localfile, ignoreMissing=ignoreMissing))
    }

    # Now, special local files
    if (tools::file_ext(i) == 'zip' ||
        tools::file_ext(i) == 'gz' ||
        tools::file_ext(i) == 'tar' ||
        tools::file_ext(i) == 'bz' 
        ) {
        if (verbose) message('archive')
        archiveFiles <- extractArchive(i, ignoreMissing=ignoreMissing)
        return(listMatchingFiles(archiveFiles, ignoreMissing=ignoreMissing))
    }

    #  At this point, it may be a simple local file or a glob pattern, but as
    #  above, we have no way of telling a priori whether this is the case
    if (lastRound) {
        #  We get to this point if the path wasn't to some file that needed
        #  special treatment (zip, remote, etc.) and it was treated as a glob
        #  pattern, which means that it is definitely not a glob pattern this
        #  time
        if (!(ignoreMissing || file.exists(i))) stop("File", i, "does not exist.")
        if (verbose) message('regular file')
        return(i)
    }
    else {
        #  If it wasn't a glob pattern last time, then it may be this time
        if (verbose) message('possible glob pattern')
        i <- Sys.glob(i)
        return(
           listMatchingFiles(i, ignoreMissing=ignoreMissing, lastRound=T)
        )
    }

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
        if (e == 'subscript out of bounds') {
            stop(paste('Unsupported extension', extension, 'of file', f))
        }
        else {
            stop(e)
        }
    })

    newSource <- switch(fileType, 
               txt = get_txt(f, ...),
               csv = get_csv(f, textField, sep=',', ...),
               tsv = get_csv(f, textField, sep='\t', ...),
               json = get_json(f, textField, ...),
               xml = get_xml(f, textField, ...)
        )

    names(newSource$txts) <- rep(basename(f), length(newSource$txts))

    return(newSource)
}

get_txt <- function(f, ...) {
    txt <- paste(readLines(con <- file(f, ...), warn = FALSE), collapse="\n")
    close(con)
    list(txts = txt, docv = data.frame())
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
#  it looks like a twitter json file
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
    txt <- readLines(path, warn = FALSE, ...)
        
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

    lines <- readLines(path, warn = FALSE)

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
