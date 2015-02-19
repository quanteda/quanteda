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


#' @export
setGeneric("textfile", 
           function(file=NULL, textIndex=NULL, directory=NULL) standardGeneric("textfile"))

#' @export
setMethod("textfile", 
          signature(file = "character", textIndex = "ANY", directory = "missing"),
          definition = function(file, textIndex = NULL) {
              fileType <- getFileType(file)
              if (fileType == "excel") {
                  if (is.null(textIndex))
                      stop("You must specify a text column name or column number identifying the texts.")
                  tmp <- xlsx::read.xlsx2(file, stringsAsFactors=FALSE, ...)
                  return(new("corpusSource", texts = tmp[, textIndex], docvars = tmp[, -textIndex]))
              } else {
                  stop(fileType, "not implemented yet.")
              }
                  
          })


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
readExcel <- function(path=NULL, sheetIndex=1) {
    if(!requireNamespace("xlsx", quietly = TRUE)) {
        stop("The xlsx package is needed for this function to work. Please install it.",
             call. = FALSE)
    }
    # choose it from a GUI if none exists
    if (is.null(path)) {
        if (require(tcltk2))
            texts <- tcltk::tk_choose.dir()
        if (is.na(texts)) stop("Directory selection cancelled by user.")
        else
            stop("you need tcltk2 installed to use GUI directory selection.")
    }
    stopifnot(class(path) == "character")
    stopifnot(file.exists(path))
    
    sheet <- xlsx::read.xlsx2(path,  stringsAsFactors=FALSE, sheetIndex=sheetIndex)
    class(sheet) <- (list("excel","data.frame"))
    
    return(sheet)
}

         
         
    
         