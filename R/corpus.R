#'Constructor for corpus objects
#'
#'
#' This function creates a corpus from a character vector (of texts), 
#' adds text-specific variables (which we term "attributes"), along
#' with optional meta-data and notes.
#' 
#' @param texts A character vector. This may be either a filepath to a directory containing text documents,
#' or a character vector containing texts. file.exists checks if it is a valid path
#' @param docnames Names to be assigned to the texts, defaults to the names of the 
#' character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param attribs A data frame of attributes that is associated with each text.
#' @param attribsIn Specifies if the attributes and values are encoded in file names, directory names, or headers
#' values are 'filenames', 'dirnames', or 'headers'
#' @param sep Separator for attribute values if specified in file names or directory names
#' @param source A string specifying the source of the texts, used for referencing.
#' @param notes A string containing notes about who created the text, warnings, To Dos, etc.
#' todo examples
#' @export
corpus <- function(texts, ...){
  UseMethod("corpus")
}


#' @export
corpus.default <- function(){
  require(tcltk2) 
  texts <- tk_choose.dir()
  return(corpus.character(texts))
}

# constructor for simple class representing a directory
#' @export
getDirectory <- function(path){
  stopifnot(class(path)=="character")
  stopifnot(file.exists(path))
  tempPath <- path
  class(tempPath) <- list("directory",class(tempPath))
  return(tempPath)
}


#' @export
corpus.directory<- function(path, docnames=NULL, attribs=NULL, attNames=NULL, sep='_', metadata=NULL, 
                            notes=NULL, citation=NULL){
  if (!class(path)[1]=="directory") stop("path must be a directory")
  texts <- getTextDir(path)
  if(class(attribs)=='character'){
    stopifnot(attribs=='filenames' | attribs=='headers')
    if(attribs=='filenames'){
      fnames <- list.files(path, full.names=TRUE)
      snames <- getRootFileNames(fnames)
      snames <- gsub(".txt", "", snames)
      parts <- strsplit(snames, sep)
      attributesdf <-  data.frame(matrix(unlist(parts), nrow=length(parts), 
                                         byrow=TRUE), stringsAsFactors=FALSE)
      if (ncol(attributesdf) != length(attNames)) {
        stop("The length of the parts of the filename does not equal the length of the attribute names")
      }
      names(attributesdf) <- attNames
      attribs=attributesdf
    }
  
  }

  return(NextMethod(texts=texts, docnames=docnames, attribs=attribs,
                    metadata=metadata, notes=notes, citation=citation))
}


#' This function creates a corpus from a character vector (of texts), 
#' adds text-specific variables (which we term "attributes"), along
#' with optional meta-data and notes.
#' 
#' @param texts A character vector containgin
#' @param docnames Names to be assigned to the texts, defaults to the names of the 
#' character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param attribs A data frame of attributes that is associated with each text.
#' @param source A string specifying the source of the texts, used for referencing.
#' @param notes A string containing notes about who created the text, warnings, To Dos, etc.
#' @export
#' @examples
#' summary(budgets)
corpus.character <- function(texts, docnames=NULL, attribs=NULL,
                             metadata=NULL, notes=NULL, citation=NULL, ...) {
  # name the texts vector
  if (!is.null(docnames)) {
    stopifnot(length(docnames)==length(texts))
    names(texts) <- docnames
  } else if (is.null(names(texts))) {
    names(texts) <- paste("text", 1:length(texts), sep="")
  }
  
  # create document-meta-data
  if (is.null(source)) {
    source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
  }
  created <- date()
  metadata <- c(source=source, created=created, notes=notes, citation=citation)
  
  # user-supplied document-level meta-data
  if (!is.null(attribs)) {
    stopifnot(nrow(attribs)==length(texts))
    attribs <- cbind(data.frame(texts, row.names=names(texts), 
                                check.rows=TRUE, stringsAsFactors=FALSE),
                     data.frame(attribs, row.names=names(texts),
                                check.rows=TRUE, check.names=TRUE))

  } else {
    attribs <- data.frame(texts, row.names=names(texts), 
                          check.rows=TRUE, stringsAsFactors=FALSE)
  }  
  # build the corpus object
  tempCorpus <- list(attribs=attribs, 
                      metadata=metadata
  )
  class(tempCorpus) <- list("corpus", class(tempCorpus))
  return(tempCorpus)
}

# accessor for texts
#' @export
texts <- function(corp){
  return(unlist(corp$attribs$texts))
}

# replacement function for texts
# warning about no data
#' @export
"texts<-" <- function(corp, value){
  corp$attribs$texts <- value
  return(corp)
}

# accessor for data
#' @export
data <- function(corp){
  return(corp$attribs[2:length(corp$attribs)])
}

# replacement function for data
#' @export
"data<-" <- function(corp, value){
  corp$attribs[2:length(corp$attribs)]<- value
  return(corp)
}

# accessor for tokens
#' @export
tokens <- function(corp){
  return(corp$attribs$tokens)
}

# replacement function for tokens
#' @export
"tokens<-" <- function(corp, value){
  corp$attribs$tokens <- value
  return(corp)
}

#' @export
types <- function(corp) {
  return(unique(unlist(tokens(corp))))
}

# accessor for docnames
#' @export
docnames <- function(corp){
  return(names(texts(corp)))
}

# replacement function for docnames
#' @export
"docnames<-" <- function(corp, value){
  names(texts(corp)) <- value
  return(corp)
}

# length
#' @export
length.corpus <- function(corp){
  return(length(texts(corp)))
}

# accessor for language
#' @export
language <- function(corp){
  return(corp$metadata$language)
}

# replacement function for language
#' @export
"language<-" <- function(corp, value){
  corp$metadata$language <- value
  return(corp)
}

# accessor for encoding
#' @export
encoding <- function(corp){
  return(Encoding(corp$texts))
}

# replacement function for encoding
#' @export
"encoding<-" <- function(corp, value){
  texts(corp) <- Encoding(texts(corp), value)
  return(corp)
}

#' Corpus sampling
#'
#' Takes a random sample of the specified size from a corpus, with or without replacement
#' 
#' @param corpus An existing corpus to be sampled
#' @param size A positive number, the number of texts to return
#' @param replace Should sampling be with replacement?
#' @param prob Not implemented
#' @export
#' @examples
#' data(movies)
#' movieSamp <- sample(movies, 200, replace=TRUE)
sample.corpus <- function(corpus, size=n, replace=FALSE, prob=NULL){
  if(!is.null(prob)) stop("prob argument is not implemented for corpus")
  atts <- corpus$attribs
  sampleInds <- sample(nrow(atts), size=size, replace=replace)
  newAtts <- atts[sampleInds,]
  newTexts <- newAtts[[1]]
  newAtts <- newAtts[2:length(newAtts)]
  newCorp <- corpusCreate(newTexts, newAtts)
  newCorp$metadata["created"] <- paste(newCorp$metadata["created"], "sampled from",
                                       corpus$metadata["source"], collapse= " ")
  return(newCorp)
}

#' @export
print.corpus <- function(corpus){
  print("Texts (first 20): \n")
  print(paste(substr(texts(corpus)[0:20],0,100), '....'))
  print("Attributes: \n")
  d <- data(corpus)
  print(d[0:20, 2:ncol(d)])
}


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
    rows <- eval(subsetExpr, corpus$attribs, parent.frame())
    if (!is.logical(rows)) 
      stop("'subset' must evaluate to logical")
    rows <- rows & !is.na(rows)
  }
  
  if (is.null(selectExpr)) 
    vars <- TRUE
  else {
    nl <- as.list(seq_along(corpus$attribs))
    names(nl) <- names(corpus$attribs)
    vars <- c(1, eval(selectExpr, nl, parent.frame()))
  }
  # implement subset, select, and drop
  corpus$attribs <- corpus$attribs[rows, vars, drop=drop]
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
#' data(iebudgets)
#' iebudgets2010 <- subset(iebudgets, year==2010)
#' summary(iebudgets2010)
#' iebudgetsLenihan <- subset(iebudgets, speaker="Lenihan", select=c(speaker, year))
#' summary(iebudgetsLenihan)
#' }
subset.corpus <- function(corpus, subset=NULL, select=NULL) {
  tempcorp <- corpus.subset.inner(corpus, substitute(subset), substitute(select))
  return(tempcorp)
}
