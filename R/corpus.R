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
#' budgets <- corpus.create(texts, attribs=newattribs)
corpus.create <- function(texts, textnames=NULL, attribs=NULL, source=NULL, notes=NULL, attribs.labels=NULL) {
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


#' This function adds a named list of attributes to an existing corpus
#' 
#' @param corpus Corpus to add attributes to
#' @param newattribs A list of new attribues should be a named list of length(corpus$texts)
#' @param name A name for the new attribues
#' @export
corpus.add.attributes <- function(corpus, newattribs, name=newattribs) {
  newattribs <- as.data.frame(newattribs, stringsAsFactors=FALSE)
  names(newattribs) <- name
  corpus$attribs <- cbind(corpus$attribs, newattribs)
  return(corpus)
}

#' create text from a string
#'
#' This function associates a string of text with a list of attribute:value pairs
#' 
#' @param text The string of text
#' @param fname The name of the file containing the text
#' @param atts A data frame of attributes that can be associated with each
#' text
#' @export
#' @examples
#' Cowen05 <- create.text("this is a speech",'Cowen05.txt', atts=cowen.atts)
create.text <- function(string, fname, atts=NULL){
  # a text has a list of attribute:value pairs
  if(is.null(atts))
  {
    atts <- list(fname=fname)   
  }
  temp.text <- list(string=string, atts=atts)
  class(temp.text) <- list("text", class(temp.text))
  return(temp.text)
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
#' budgets <- corpus.append(budgets, texts, newattribs)
corpus.append <- function(corpus1, newtexts, newattribs, ...) {
  # 
  # should make it also allow an optional corpus2 version where two
  # corpuses could be combined with corpus.append(corp1, corp2)
  # if we can verify the same attribute set.
  tempcorpus <- corpus.create(newtexts, attribs=newattribs)
  corpus1$attribs <- rbind(corpus1$attribs, tempcorpus$attribs)
  #corpus1$attribs$texts <- rbind(corpus1$attribs$texts, tempcorpus$attribs$texts)
  # TODO: implement concatenation of any attribs.labels from new corpus
  return(corpus1)
}

#' Display a summary of a corpus object

#' Displays information about a corpus object, including attributes and 
#' metadata such as date of number of texts, creation and source
#' 
#' @param corpus An existing corpus to be summarized
#' @param texts The name of the attribute containing the corpus texts, if
#' not 'texts'
#' @export
#' @examples
#' summary.corpus(corpus1)
####### KB: NEED TO FIX THIS TO DISPLAY A SUMMARY EVEN WHEN is.null(attribs)
#######     fixed 19:00 26 June 2013
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
  #'@export
    
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

corpus.subset <- function(corpus, subset=NULL, select=NULL) {
  tempcorp <- corpus.subset.inner(corpus, substitute(subset), substitute(select))
  return(tempcorp)
}


#' Transform a corpus by splitting texts into sentences

#' Each text in the corpus is split into sentences, and each
#' sentence becomes a standalone text, with attributes indicating
#' the text it is taken from and it's serial number in that text
#' 
#' @param corpus Corpus to transform
#' @param feature Feature to count
#' @examples
#' sentCorp <- corpus.reshape(corpus)
corpus.reshape <- function(corpus){
  sentence <- sentenceSeg(corpus$attribs$texts[[1]])
  sentenceno <- 1:length(sentence)
  sourcetext <- rep(row.names(corpus$attribs)[[1]], length(sentence))
  atts <- data.frame(sourcetext, sentenceno)
  # print(names(atts))
  sentCorp <- corpus.create(unlist(sentence), attribs=atts)
  # print(names(sentCorp$attribs))
  for(i in 2:nrow(corpus$attribs)){
    sentence <- sentenceSeg(corpus$attribs$texts[[i]])
    sentenceno <- 1:length(sentence)
    sourcetext <- rep(row.names(corpus$attribs)[[i]], length(sentence))
    atts <- data.frame(sourcetext, sentenceno)
    
    sentCorp<-corpus.append(sentCorp, unlist(sentence), atts)
  }
  return(sentCorp)
}