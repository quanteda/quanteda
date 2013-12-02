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

library(austin)
if(!require(XML)){
  print("XML package is required for translation")
}
if (!require(RCurl)) {
  print("RCurl package is required for translation")
}

#' Truncate absolute filepaths to root filenames
#'
#' This function takes an absolute filepath and returns just the 
#' document name
#'
#' @param longFilenames Absolute filenames including a full path with directory
#' @export
#' @examples
#' getRootFilnames('/home/paul/documents/libdem09.txt')
getRootFileNames <- function(longFilenames) {
  ## function to return just the filename, path not included
  ## might need to detect .Platform$OS.type to change the delimiter
  delim <- "/"
  osName <- (Sys.info()[['sysname']] )
  if(osName=="Windows") { delim <- "\\\\" }
  
  splitFilenames <- strsplit(longFilenames, delim)
  return(sapply(splitFilenames, tail, n=1))
}


#' load text files from disk into a vector of character vectors

#' points to files, reads them into a character vector of the texts
#' with optional names, default being filenames
#' returns a named vector of complete, unedited texts
#' 
#' @param filenames 
#' @export
#' @examples
#' getTextFiles('/home/paul/documents/libdem09.txt')
getTextFiles <- function(filenames, textnames=NULL) {
  # TODO detect encoding; verbose=TRUE (progress bar?)
  print(filenames)
  textsvec <- c()  
  # changed from readChar to readLines
  for (f in filenames) {
    textsvec = c(textsvec, paste(readLines(file(f)), collapse="\n")) 
  }
  # name the vector with the filename by default, otherwise assign "names"
  ifelse(is.null(textnames), 
         names(textsvec) <- getRootFileNames(filenames),
         names(textsvec) <- textnames)
  return(textsvec)
}


#' loads all text files from a given directory 
#'
#' given a directory name, get a list of all files in that directory
#' and load them into a character vector using getTextFiles
#' 
#' 
#' @param dirname A directory path
#' @export
#' @examples
#' getTextdir('/home/paul/documents/')
getTextDir <- function(dirname) {
  # get all files from a directory
  return(getTextFiles(list.files(dirname, full.names=TRUE)))
}


#' provides a gui interface to choose a gui to load texts from
#'
#' launches a GUI to allow the user to choose a directory from
#' which to load all files.
#' @export
#' @examples
#' getTextFiles('/home/paul/documents/libdem09.txt')
getTextDirGui <- function() {
  files <- choose.files()
  #get all files from a directory
  return(getTextFiles(files))
}


#' Imports a Wordstat corpus from a CSV file
#'
#' Reads in a wordstat CSV file and creates a corpus object
#' with the document as text and variables as attributes
#' @export
#' @examples
#' 
#' 
getWordStatCSV <- function(filename=NULL) {
  f <- ifelse(is.null(filename), file.choose(), filename)
  doc <- read.csv(f, stringsAsFactors=FALSE)
  text <- doc$DOCUMENT
  atts <- subset(doc, select=-c(DOCUMENT))
  c <- corpus.create(text, attribs=atts)
  return(c)
}


#' Imports a Wordstat corpus from an XML file
#'
#' Reads in a wordstat XML file and creates a corpus object
#' with the document as text and variables as attributes
#' @export
#' @examples
#' 
getWordStat <- function(filename=NULL) {
  require(XML)
  f <- ifelse(is.null(filename), file.choose(), filename)
  str <- paste(readLines(f), collapse=" ")
  str <- gsub('<codebook>','',str)
  str <- gsub('</codebook>','',str)
  doc <- xmlRoot(xmlTreeParse(str))
  # the first two nodes are description and codebook - structure holds all the cases and vars
  struct <- doc[['structure']]
  
  # the variables are declared before the cases but at the same level - have to filter them out
  vars <- Filter(function(x) xmlName(x)=="variable", xmlChildren(struct))
  children <- Filter(function(x) xmlName(x)=="case", xmlChildren(struct))
  texts <- rep(NA, length(children))
  init <- rep(NA, length(children[[1]])-1)
  atts <- as.data.frame(t(init))
  
  # for each case we take out the document and the variable values
  for(i in 1:length(children)){
    x <- children[[i]]
    vals <- sapply(x, xmlValue)
    ns <- sapply(x, xmlName)
    names(vals) <- ns
    texts[[i]] <- vals['DOCUMENT']
    vals <- x[-which(names(x)=='DOCUMENT')]
    vals <- sapply(vals, xmlValue)
    names(atts) <- names(vals)
    atts <- rbind(atts, vals)
    atts <- na.omit(atts)
  }
  c <- corpus.create(texts, attribs=atts)
  return(c)
}

#' print a summary of texts 

#' Prints to the console a desription of the texts, including 
#' number of types, tokens, and sentences
#' 
#' @param texts
#' @export
#' @examples
#' describeTexts(texts)
describeTexts <- function(texts, output=TRUE) {
  # need to implement subsetting here too
  string <- gsub("[[:punct:][:digit:]]", "", texts)
  string <- gsub("\n", "", string)
  string <- string[string!=""]
  string <- tolower(string)
  tokenized.string <- lapply(string, function(s) strsplit(s, c(" ", ".", "?", "!")))
  ntokens <- sapply(tokenized.string, function(s) sapply(s, length))
  ntypes  <- sapply(tokenized.string, function(s) sapply(s, function(s2) length(unique(s2))))
  nsents  <- sapply(texts, function(s) length(gregexpr("[.!?]", s)[[1]]))
  if (output) {
    cat("Summary of texts:\n", substitute(texts), sep="")
    print(data.frame(Texts=names(texts),
                     Types=ntypes,
                     Tokens=ntokens,
                     Sentences=nsents,
                     row.names=NULL))
  }
  return(invisible(list(ntokens=ntokens, ntypes=ntypes, nsents=nsents)))
}



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
  } else attribs <- data.frame(texts=texts, attribs,
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
#' @param newattribs A list of new attribues
#' @param name A name for the new attribues
#' @export
corpus.add.attributes <- function(corpus, newattribs, name=newattribs) {
  # attribs should be a named list of length(corpus$texts)
  # can be one or more variables
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


#### Output a WEKA-compatible arff file for an fvm
#### still in development
####
create.arff <- function(fvm, gold, name="politics", outfile="test.arff"){
  outString <- paste("@RELATION", name,"\n")
  
  for(c in rownames(fvm)){
    outString <- paste(outString, "@attribute", c, "NUMERIC\n")
  }
  outString <- paste(outString, "@attribute class {Economic, Social, Neither}\n")
  outString <- paste(outString, "@DATA\n")
  i<-1
  while(i<length(fc)){
    line <- paste(fc[, i], collapse=",")
    outString <- paste(outString, line)
    outString <- paste(outString, ",")
    outString <- paste(outString, gold[[i]])
    outString <- paste(outString, "\n")
    i <- i+1
    print(i)
  }
  
  writeChar(outString, file("/home/paul/testout.arff"))
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


#' Create a feature-value matrix from a corpus object

#' returns a feature value matrix compatible with austin
#' 
#' @param corpus Corpus to make matrix from
#' @param feature Feature to count
#' @param feature type to aggregate by, default is file
#' @export
#' @examples
#' fvm <- create.fvm.corpus(budgets, group="party")
create.fvm.corpus <- function(corpus,
                              feature=c("word"),
                              stem=FALSE,
                              remove_stopwords=FALSE,
                              groups=NULL,
                              subset=NULL, 
                              verbose=TRUE) {
  # default is to take word as features, could expand that
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
  
  #save(texts, file="temptexts")
  fvm <- data.frame(feature=NA)
  progress.threshold <- .1
  
  if (verbose) cat("  Progress (%): [0")
  
  for (i in 1:length(texts)) {
    # if(i%%10==0) cat(i)
    if (i/length(texts) > progress.threshold) {
      if (verbose) cat(paste("...", progress.threshold*100, sep=""))
      progress.threshold <- progress.threshold+.1
    }
    #tokenized.txt <- tokenize(texts[i])
    text = clean(texts[i])
    tokenized.txt <- scan(what="char", text=text, quiet=TRUE)
    # flush out "empty" strings caused by removal of punctuation and numbers
    tokenized.txt <- tokenized.txt[tokenized.txt!=""]
    if (stem==TRUE) {
      require(SnowballC)
      tokenized.txt <- wordStem(tokenized.txt)
    }
    wfTable <- as.data.frame(table(tokenized.txt))
    if(length(tokenized.txt)>0){
      names(wfTable) <- c("feature", textnames[i])
    }

    fvm <- merge(fvm, wfTable, by="feature", all=TRUE)
  }
  
  # convert NAs to zeros
  fvm[is.na(fvm)] <- 0
  # drop any words that never occur
  fvm <- fvm[-which(apply(fvm[,-1], 1, sum)==0),]
  if (verbose) cat("]\n")
  # make rownames the feature name, and remove the feature name as a data column
  rownames(fvm) <- fvm[,"feature"]
  fvm <- fvm[,-1]
  
  if (verbose) cat("\n")
  if(remove_stopwords){
    data(stopwords_EN)
    stopwords <- stopwords_EN
    stopwfm <- as.wfm(subset(fvm, !row.names(fvm) %in% stopwords))
    fvm <- stopwfm
  }
  return(fvm)
}

##
## Ken's ongoing efforts to get the bleedin' subsetting function working!!!!
## AND SUCCESS!!!! YEAAAAHHH!!!!
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


#' make a corpus object from results of a twitter search

#' All of the attributes returned by the twitteR
#' library call are included as attributes in the
#' corpus. A oauth key is required, for further
#' instruction about the oauth processs see:
#' https://dev.twitter.com/apps/new
#' and the twitteR documentation
#' 
#' @param query Search string for twitter
#' @param oauth Oauth key
#' @param numResults Number of results desired.
#' @examples
#' sentCorp <- corpus.reshape(corpus)
twitterTerms <- function(query, oauth, numResults=50){
  library('twitteR')
  registerTwitterOAuth(oauth)
  sea <- (searchTwitter(query, numResults))
  atts <- results[2:nrow(results),]
  twc <- corpus.create(texts, attribs=t(atts))
  return(twc)
}




#' Create a feature-value matrix from a corpus object
#' Development version for optimization PN 26 Nov 13

#' returns a feature value matrix compatible with austin
#' 
#' @param corpus Corpus to make matrix from
#' @param feature Feature to count
#' @param feature type to aggregate by, default is file
#' @export
#' @examples
#' fvm <- create.fvm.corpus(budgets, group="party")
create.fvm.matrix.corpus <- function(corpus, verbose=TRUE){
  if (verbose) cat("Creating fvm (optimized):\n")
  texts <- corpus$attribs$texts
  names(texts) <- rownames(corpus$attribs)
  tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
  print(names(tokenizedTexts))
  tokens <- unlist(tokenizedTexts)
  types <- unique(tokens)
  dnames<-list(c(docs=names(texts)), c(words=types))
  fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
  i=1
   while(i<=length(texts)){
     curTable = table(tokenizedTexts[i])
     curTypes <- names(curTable)
     print(i)
     j<-1
     for (type in types){
        fvm[i,j]<- curTable[type]
        j <- j+1
      }
    i <- i+1
  }
  fvm[is.na(fvm)] <-0             
  return(fvm)
}

create.fvm.plyr.corpus <- function(corpus, verbose=TRUE){
  if (verbose) cat("Creating fvm (optimized):\n")
  texts <- corpus$attribs$texts
  names(texts) <- rownames(corpus$attribs)
  tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
  print(names(tokenizedTexts))
  tokens <- unlist(tokenizedTexts)
  types <- unique(tokens)
  dnames<-list(c(docs=names(texts)), c(words=types))
  fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
  fvm <- as.data.frame(fvm)
  i=1
  while(i<=length(texts)){
    print(i)
    curTable = table(tokenizedTexts[i])
    cm <- as.matrix(curTable)
    cm <- t(cm)
    cd <- as.data.frame(cm)
    invisible(fvm <- suppressMessages(match_df(fvm, cd)))
    i <- i+1
  }
  fvm[is.na(fvm)] <-0              
  return(fvm)
}

create.fvm.adder.corpus <- function(corpus, verbose=TRUE){
  if (verbose) cat("Creating fvm (optimized):\n")
  texts <- corpus$attribs$texts
  names(texts) <- rownames(corpus$attribs)
  tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
  print(names(tokenizedTexts))
  tokens <- unlist(tokenizedTexts)
  types <- unique(tokens)
  dnames<-list(c(docs=names(texts)), c(words=types))
  fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
  i=1
  Rprof(append = FALSE)
  while(i<=length(texts)){
    curText = unlist(tokenizedTexts[i])
    print(i)
    j<-1
    while(j<=length(curText)){
      word <- types[j]
      fvm[i,curText[j]]<- fvm[i,curText[j]]+1
      j <- j+1
    }
    i <- i+1
  }
  fvm[is.na(fvm)] <-0   
}
