###
### design of a corpus object
###
# (1) (removed)
# (2) attributes: a named list (data-frame) of "variables" or chracteristics
#     of each text.  The first columbn of this is 
#     "texts": a named vector of texts whose only treatment is conversion to unicode
# (3) attributes labels: an optional user-supplied list of descriptions of each
#     attribute
# (4) meta-data: character vector consisting ofL
#     source (user-supplied or default is full directory path and system)
#     creation date (automatic)
#     notes (default is NULL, can be user-supplied)


#' create a new corpus - a list containing texts and named attributes

#' This function takes a text (in the form of a character vectors),
#' performs some cleanup, and splits the text on whitespace, returning
#' a dataframe of words and their frequncies
#' 
#' @param texts Text to be tokenized
#' @param textnames Names to assign to the texts
#' @param attribs A data frame of attributes that can be associated with each
#' @export
#' @examples
#' data(ieTexts)
#' data(ieAttribs)
#' budgets <- corpusCreate(ieTexts, ieAttribs)
corpusCreate <- function(texts, textnames=NULL, attribs=NULL, source=NULL, notes=NULL, attribs.labels=NULL) {
  if (is.null(names(texts))) 
    names(texts) <- paste("text", 1:length(texts), sep="")
  if (is.null(source)) 
    source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
  created <- date()
  metadata <- c(source=source, created=created, notes=notes)
  if (is.null(attribs)) {
    attribs <- data.frame(texts, row.names=names(texts), 
                          check.rows=TRUE, stringsAsFactors=FALSE)
  } else attribs <- data.frame(texts, attribs,
                               row.names=names(texts), 
                               check.rows=TRUE, stringsAsFactors=FALSE)
  temp.corpus <- list(attribs=attribs,
                      metadata=metadata)
  class(temp.corpus) <- list("corpus", class(temp.corpus))
  return(temp.corpus)
}


#' create a new corpus with attribute-value pairs taken from document headers

#' This function takes a directory, reads in all the documents in that directory
#' and makes a new corpus where the attributes and values are created from
#' JSON headers in the documents. The JSON header should be the first line (as 
#' delimited by \n) in document. For example, a document may begin as follows:
#' "budgetPosition" : "1.0", "party":"FF"}
#' When I presented the supplementary budget to this House last April....
#' 
#' The directory must contain only documents to be
#' used in the corpus, and each document must have the same attributes.
#' 
#' @param directory 
#' @export
#' @examples
#' \dontrun{
#' budgets <- corpusFromHeaders("~/Dropbox/QUANTESS/corpora/withHeader")
#' }
corpusFromHeaders <- function(directory){
  library(jsonlite)
  docs <- getTextDir(directory)
  texts <- c()
  headerAttribs <- data.frame(stringsAsFactors=FALSE)
  for(d in docs){
    lines <- unlist(strsplit(d, '\n'))
    header <- data.frame(fromJSON(lines[1]), stringsAsFactors=FALSE)
    if(is.null(names(headerAttribs))){
      attribs <- data.frame(header, stringsAsFactors = FALSE)
    }
    else{
      headerAttribs <- rbind(header, headerAttribs)
    }
    content <- paste(lines[2:length(lines)], collapse='\n')
    texts <- c(texts, content) 
  }
  corp <- corpusCreate(texts, attribs=headerAttribs)
  return(corp)
}

#' create a new corpus with attribute-value pairs taken from document filenames

#' Work in progress

#' 
#' @param directory 
#' @export
#' @examples
#' \dontrun{
#' budgets <- corpusFromHeaders("~/Dropbox/QUANTESS/corpora/withHeader")
#' }
corpusFromFilenames <- function(directory, attNames, sep='_'){
  
  texts <- c()
  allAttribs <- data.frame(stringsAsFactors=FALSE)
  filenames <- list.files(directory, full.names=TRUE)
  for (f in filenames) {
    text <-  paste(suppressWarnings(readLines(f)), collapse="\n")
    sname <- getRootFileNames(f)
    sname <- gsub(".txt", "", sname)
    parts <- unlist(strsplit(sname, sep))
    if(length(allAttribs) < 1)){
      allAttribs <- data.frame(attNames, stringsAsFactors = FALSE)
    }
    else{
      allAttribs <- rbind(header, headerAttribs)
    }
    
    if(length(parts)!=length(attNames)){
      stop("The length of the parts of the filename does not equal the length of the attribute names")
    }
    newattribs <- data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
    names(newattribs) <- attNames
  }
  corp <- corpusCreate(texts, attribs=newattribs)
  return()
}


#' This function adds a named list of attributes to an existing corpus
#' 
#' @param corpus Corpus to add attributes to
#' @param newattribs A list of new attributes should be a named list of length(corpus$texts)
#' @param name A name for the new attribues
#' @return corpus A corpus with the new attributes added
#' @export
corpusAddAttributes <- function(corpus, newattribs, name=newattribs) {
  newattribs <- as.data.frame(newattribs, stringsAsFactors=FALSE)
  names(newattribs) <- name
  corpus$attribs <- cbind(corpus$attribs, newattribs)
  return(corpus)
}


#' function to add new texts and attributes to an existing corpus

#' Accepts a list of texts and a list of associated attributes and 
#' adds them to the corpus
#' 
#' @param corpus An existing corpus to add new texts and attributes to
#' @param newtexts New texts to be added to the corpus
#' @param newattribs New attribs associated with the new texts
#' text
#' @export
#' @examples
#' data(iebudgets)
#' data(ieAttribs)
#' data(ieTexts)
#' budgets <- corpusAppend(iebudgets, ieTexts, ieAttribs)
corpusAppend <- function(corpus1, newtexts, newattribs, ...) {
  # 
  # should make it also allow an optional corpus2 version where two
  # corpuses could be combined with corpus.append(corp1, corp2)
  # if we can verify the same attribute set.
  tempcorpus <- corpusCreate(newtexts, attribs=newattribs)
  corpus1$attribs <- rbind(corpus1$attribs, tempcorpus$attribs)
  #corpus1$attribs$texts <- rbind(corpus1$attribs$texts, tempcorpus$attribs$texts)
  # TODO: implement concatenation of any attribs.labels from new corpus
  return(corpus1)
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


#' Transform a corpus by splitting texts into sentences
#'
#' Each text in the corpus is split into sentences, and each
#' sentence becomes a standalone text, with attributes indicating
#' the text it is taken from and it's serial number in that text
#' 
#' @param corpus Corpus to transform
#' @param feature Feature to count
#' @examples
#' \dontrun{
#' corpus <- data(iebudgets)
#' sentCorp <- corpus.reshape(corpus)
#' }
corpusReshape <- function(corpus) {
  sentence <- sentenceSeg(corpus$attribs$texts[[1]])
  sentenceno <- 1:length(sentence)
  sourcetext <- rep(row.names(corpus$attribs)[[1]], length(sentence))
  atts <- data.frame(sourcetext, sentenceno)
  sentCorp <- corpusCreate(unlist(sentence), attribs=atts)
  for(i in 2:nrow(corpus$attribs)){
    sentence <- sentenceSeg(corpus$attribs$texts[[i]])
    sentenceno <- 1:length(sentence)
    sourcetext <- rep(row.names(corpus$attribs)[[i]], length(sentence))
    atts <- data.frame(sourcetext, sentenceno)
    
    sentCorp<-corpusAppend(sentCorp, unlist(sentence), atts)
  }
  return(sentCorp)
}

#' Display a summary of a corpus object
#'
#' Displays information about a corpus object, including attributes and 
#' metadata such as date of number of texts, creation and source
#' 
#' @param corpus An existing corpus to be summarized
#' @param texts The name of the attribute containing the corpus texts, if
#' not 'texts'
#' @export
#' @examples
#' data(iebudgets)
#' summary(subset(iebudgets, year==2010))
summary.corpus <- function(corpus, texts="texts", subset=NULL, select=NULL, drop=FALSE, output=TRUE, nmax=100) {
  corpus <- corpus.subset.inner(corpus, substitute(subset), substitute(select))
  cat("Corpus object contains", nrow(corpus$attribs), "texts.\n\n")
  # allow user to set the column or variable which identifies the texts to summarize
  texts <- corpus$attribs[,texts]
  attribs <- as.data.frame(corpus$attribs[,-1])
  #print(as.character(substitute(select))[2])
  if (ncol(attribs)==1) names(attribs) <- as.character(substitute(select))[2]
  #print(names(attribs))
  names(texts) <- rownames(corpus$attribs)
  print(head(cbind((dtexts <- describeTexts(texts, output=FALSE)),
                   attribs), 
             nmax))
  cat("\nSource:  ", corpus$metadata["source"], ".\n", sep="")
  cat("Created: ", corpus$metadata["created"], ".\n", sep="")
  cat("Notes:   ", corpus$metadata["notes"], ".\n\n", sep="")
  # invisibly pass the summary of the texts from describetexts()
  return(invisible(dtexts))
}



#
# FUNCTIONS BELOW THIS POINT ARE DEPRECATED
#



#' Create a feature-value matrix from a corpus object

#' returns a feature value matrix compatible with austin
#' 
#' @param corpus Corpus to make matrix from
#' @param feature Feature to count
#' @param feature type to aggregate by, default is file
#' @export
#' @examples
#' \dontrun{
#' fvm <- create.fvm.corpus(budgets, group="party")
#' }
create.fvm.corpus <- function(corpus,
                              feature=c("word"),
                              stem=FALSE,
                              remove_stopwords=FALSE,
                              groups=NULL,
                              subset=NULL, 
                              verbose=TRUE) {
  if (verbose) cat("Creating fvm:\n")
  # new subset feature (no "select" because inappropriate here)
  corpus <- corpus.subset.inner(corpus, substitute(subset))
  # new aggregation - to concatenate texts before making the fvm 
  if (!is.null(groups)) {
    if (verbose) cat("  Now aggregating by group: ", groups, "...", sep="")
    if (length(groups)>1) {
      group.split <- lapply(corpus$attribs[,groups], as.factor)
    } else group.split <- as.factor(corpus$attribs[,groups])
    texts <- split(corpus$attribs$texts, group.split)
    # was sapply, changing to lapply seems to fix 2 class case
    texts <- lapply(texts, paste)
    if (verbose) cat("complete.\n")
  } else {
    texts <- corpus$attribs$texts
    names(texts) <- rownames(corpus$attribs)
  }
  textnames <- names(texts)
  tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
  if (stem==TRUE) {
    require(SnowballC)
    tokenizedTexts <- wordStem(tokenizedTexts)
  }
  tokens <- unlist(tokenizedTexts)
  types <- unique(tokens)
  dnames<-list(c(docs=names(texts)), c(words=types))
  fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
  i=1
  prog=0
  if(verbose) cat("Progress:          ")
  while(i<=length(texts)){
    if(verbose){
      # erase and redraw progress meter.
      cat("\b\b\b\b\b\b\b\b\b\b")
      cat(sprintf("[%6.2f%%] ",prog))
    }
    curTable = table(tokenizedTexts[i])
    curTypes <- names(curTable)
    # indexing the table is faster with 'type' than 'types[j]' but indexing 
    # the fvm is faster with j, so use both counter and for-loop
    j<-1
    for(type in types){
      fvm[i,j]<-curTable[type]
      j<-j+1
    }
    i <- i+1
    prog <- (i/length(texts)*100) 
  }
  if(verbose) cat(" Done. \n")
  # convert NAs to zeros
  fvm[is.na(fvm)] <- 0
  fvm <- t(fvm)
  fvm <- as.data.frame(fvm)
  if(remove_stopwords){
    data(stopwords_EN)
    stopwords <- stopwords_EN
    stopwfm <- as.wfm(subset(fvm, !row.names(fvm) %in% stopwords))
    fvm <- stopwfm
    
  }
  return(fvm)
}
