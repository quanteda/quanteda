#' corpus source classes 
#' 
#' The \code{corpusSource} virtual class is a parent class for more specific 
#' corpus source objects.
#' 
#' @slot texts the texts that form the core of the corpus
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
#' @param file the complete filename to be read
#' @param textField a variable (column) name or column number indicating where 
#'   to find the texts that form the documents for the corpus
#' @param directory not used yet, and may be removed (if I move this to a new 
#'   method called \code{textfiles})
#' @details The constructor does not store a copy of the texts, but rather reads
#' in the texts and associated data, and saves them to a temporary R object
#' whose location is specified in the \link{corpusSource-class} object.  This
#' prevents a complete copy of the object from cluttering the global environment
#' and consuming additional space.  This does mean however that the state of the
#' file containing the source data will not be cross-platform and may not be
#' persistent across sessions.  So the recommended usage is to load the data
#' into a corpus in the same session in which \code{textfile} is called.
#' @return an object of class \link{corpusSource-class} that can be read by 
#'   \libk{corpus} to construct a corpus
#' @export
setGeneric("textfile", 
           function(file, textField, directory, ...) standardGeneric("textfile"),
           signature = c("file", "textField", "directory"))
# setGeneric("textfile", 
#            function(file=NULL, textField=NULL, directory=NULL, ...) standardGeneric("textfile"))

# FROM THE MATRIX PACKAGE - no need to duplicate here
# setClassUnion("index", members =  c("numeric", "integer", "logical", "character"))

#' @rdname textfile
#' @export
setMethod("textfile", 
          signature(file = "character", textField = "index", directory = "missing"),
          definition = function(file, textField, directory=NULL, ...) {
              if (length(textField) != 1)
                  stop("textField must be a single field name or column number identifying the texts.")
              fileType <- getFileType(file)
              if (fileType == "csv") {
                  docv <- read.csv(file, stringsAsFactors=FALSE, ...)
                  if (is.character(textField)) {
                      textFieldi <- which(names(docv)==textField)
                      if (length(textFieldi)==0)
                          stop("column name", textField, "not found.")
                      textField <- textFieldi
                  }
                  txts <- docv[, textField]
                  docv <- docv[, -textField]
              } else {
                  stop("file type", fileType, "not yet implemented")
              }
              
              
              tempCorpusFilename <- tempfile()
              save(txts, docv, file=tempCorpusFilename)
              new("corpusSource", texts=tempCorpusFilename)
          })


#' Constructor for corpus objects from corpusSource
#' 
#' Creates a corpus from a \code{corpusSource} object created by \link{textfile}
#' @rdname corpus
#' @export
#' @examples
#' # the fifth column of this csv file is the text field
#' mytexts <- textfile("http://www.kenbenoit.net/files/text_example.csv", textField=5)
#' str(mytexts)
#' mycorp <- corpus(mytexts)
#' mycorp2 <- corpus(textfile("http://www.kenbenoit.net/files/text_example.csv", textField="Title"))
#' identical(texts(mycorp), texts(mycorp2))
#' identical(docvars(mycorp), docvars(mycorp2))
corpus.corpusSource <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    load(x@texts, envir = environment())  # load from tempfile only into function environment
    corpus(txts, docvars=docv)
}



getFileType <- function(filenameChar) {
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
    else return("unknown") }, USE.NAMES=FALSE)
}    

    

# textfile(file=NULL, dir=NULL, textIndex = NULL, format=NULL)
#   xlsx
#   csv
#   json
#   directory
#   zip, gz, tar.gz, tar
#   
# 
# textstream
#   getTweets
#   getTimeLine
#   getFBpage
# 


# corpus(textsource(file="myfile.xlsx", ))

# Function to declare a connection to an excel file
#
# Function to declare a connection to a excel file.
#
# @param path  String describing the full path to the excel file or NULL to use 
#  a GUI to choose a directory from disk
# @param sheetIndex  The index of the sheet of the excel file to read (as passed
#  to read.xlsx2)
# @export
# readExcel <- function(path=NULL, sheetIndex=1) {
#     if(!requireNamespace("xlsx", quietly = TRUE)) {
#         stop("The xlsx package is needed for this function to work. Please install it.",
#              call. = FALSE)
#     }
#     # choose it from a GUI if none exists
#     if (is.null(path)) {
#         if (require(tcltk2))
#             texts <- tcltk::tk_choose.dir()
#         if (is.na(texts)) stop("Directory selection cancelled by user.")
#         else
#             stop("you need tcltk2 installed to use GUI directory selection.")
#     }
#     stopifnot(class(path) == "character")
#     stopifnot(file.exists(path))
#     
#     sheet <- xlsx::read.xlsx2(path,  stringsAsFactors=FALSE, sheetIndex=sheetIndex)
#     class(sheet) <- (list("excel","data.frame"))
#     
#     return(sheet)
# }

         
         
    
         