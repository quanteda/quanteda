###
### design of a corpus object
###
# (1) texts: a named vector of texts whose only treatment is conversion to unicode
# (2) attributes: a named list (data-frame) of "variables" or chracteristics
#     of each text
# (3) attributes labels: an optional user-supplied list of descriptions of each
#     attribute
# (4) meta-data: character vector consisting ofL
#     source (user-supplied or default is full directory path and system)
#     creation date (automatic)
#     notes (default is NULL, can be user-supplied)

# testing git again

library(austin)
if(!require(XML)){
  print("XML package is required for translation")
}
if (!require(RCurl)) {
  print("RCurl package is required for translation")
}

translateChunk <- function(sourceText, sourceLanguage, targetLanguage, key=NULL, verbose=TRUE) {
  if (is.null(key)) {
    key <- "DQAAALoAAABQS8Lok-tdR8rU1ewhKf1o7IJxS0m_X63cVuDI3ETGyg8rgWhgYTyaXBDdqIe1TUSlCzTbUi70iYQ5bsTOznfk_W9yXNG68iKxExrSxyy5iT5nXbRn3dXONOCcqkNHmJJ-zQAmwP4Gw3uyFEx2A_JES5Xru_Kaq2aJ9hOfRae8h4bqN_PKe7T_HRTg0xhwaNVWGno_tctoe5zXOHcRbEeRyFG-TTrD45ceJMSat7NPF2n7noIPFvL9SNpD026RSPM"
  }
  if (verbose){
    cat("Making call to Google Translate..., with string of length: ", nchar(sourceText), "\n")
    print(sourceText)
  }
  baseUrl <- "http://translate.google.com/researchapi/translate?"
  params <- paste("sl=",sourceLanguage, "&tl=", targetLanguage, "&q=", sourceText,sep="")
  
  url <- paste(baseUrl,params,sep="")
  header <- paste("Authorization: GoogleLogin auth=", key, sep="")
  # make the http requst with the url and the authentication header
  if (verbose) print(url)
  curl <- getCurlHandle()
  response <- getURL(url, httpheader=header, curl=curl)
  # get the http response code to try to see what type of error we're getting
  code <- getCurlInfo(curl, which="response.code")
  print(code)
  rm(curl)
  Sys.sleep(1)
  # parse XML response to extract actual translation
  doc <- xmlTreeParse(response, getDTD = F)
  r <- xmlRoot(doc) 
  translation <- xmlValue(r["entry"] [[1]] [[5]])
  return(translation)
}

translate.corpus <- function(corpus, targetlanguageString, 
                             textvar="texts", languagevar="language") {
  ## function to translate the text from a corpus into another language
  ## wrapper for translate
  # initialize the translated text vector
  translatedTextVector <- rep(NA, nrow(corpus$attribs))
  for (i in 1:nrow(corpus$attribs)) {
    if (corpus$attribs[i,textvar]=="" | is.na(corpus$attribs[i,textvar])) next
    if (corpus$attribs[i,languagevar]==targetlanguageString) next
    translatedTextVector[i] <- translate(corpus$attribs[i,textvar], 
                                         corpus$attribs[i,languagevar],
                                         targetlanguageString)
  }
  return(translatedTextVector)
}

translate <- function(sourceText,  sourceLanguage, targetLanguage, key=NULL, verbose=FALSE){
  a <- strsplit(sourceText, split="[\\.]")
  sentences <- unlist(a)
  # Paste sentences together into a chunk until the next one would send the current chunk
  # over 1000 chars, then send to Google.
  chunk <- ""
  translatedText <- ""
  for (i in 1:length(sentences)) {
    s <- sentences[i]
    if (nchar(s) < 2) {
      if(verbose) print("empty sentence")
      next
    }
    s <- curlEscape(s)
    # handle the rare (non-existent?) case of a single sentence being >1000 chars
    if (nchar(s) >= 1000) {
      if (verbose) print("in the 1000 case")
      start <- 1
      end <- 1000
      while ((nchar(s) - start) > 1000) {
        chunk <- substr(s, start, end)
        translatedText <- paste(translatedText, translateChunk(chunk,sourceLanguage, targetLanguage), sep=". ")
        start <- start + 1000
        end <- end + 1000
      }
      chunk <- substr(s, start, nchar(s))
      translatedText <- paste(translatedText,translateChunk(chunk,sourceLanguage, targetLanguage), sep="")
      chunk <- ""
    }
    else {
      # if this is the last sentence in the speech,
      # send it and the current chunk (if there is one) to Google
      if (i==length(sentences)) {
        if (verbose) print("one")
        #send to Google, reset the chunk
        if (nchar(chunk)>5) {
          translatedText <- paste(translatedText, translateChunk(chunk, sourceLanguage, targetLanguage),sep=". ")
        }else{
          if (verbose) print("empty chunk")
        }
        if (nchar(s)>5) {
          translatedText <- paste(translatedText, translateChunk(s, sourceLanguage, targetLanguage),sep=". ")
        } else {
          if (verbose) print("empty sentence")
        }
        chunk <- ""
      }
      #if this sentence will put the chunk over 1000, send the chunk to Google and save this sentence
      else if ((nchar(chunk)+nchar(s) >= 1000)) {
        if (verbose) print("two")
        translatedText <- paste(translatedText, translateChunk(chunk, sourceLanguage, targetLanguage), sep=". ")
        chunk <- paste(s,".%20",sep="")
      } else {
        if (verbose) print("three")
        #otherwise just add this sentence to the chunk
        chunk <- paste(chunk, s, sep=".%20")
      }
    }
  }
  translatedText <- curlUnescape(translatedText)
  if (verbose) cat("****************", translatedText, "********************", nchar(translatedText), "\n")
  if (verbose) cat("\n")
  return(translatedText)
}


getRootFileNames <- function(longFilenames) {
  ## function to return just the filename, path not included
  ## might need to detect .Platform$OS.type to change the delimiter
  delim <- "/"
  osName <- (Sys.info()[['sysname']] )
  if(osName=="Windows") { delim <- "\\\\" }
  splitFilenames <- strsplit(longFilenames, delim)
  return(sapply(splitFilenames, tail, n=1))
}

getTextFiles <- function(filenames, textnames=NULL, verbose=FALSE) {
  # points to files, reads them into a character vector of the texts
  # with optional names, default being filenames
  # return a named vector of complete, unedited texts
  # TODO detect encoding; verbose=TRUE (progress bar?)
  textsvec <- c()   # initialize text vector
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

getTextDir <- function(dirname) {
  # get all files from a directory
  return(getTextFiles(list.files(dirname, full.names = TRUE)))
}

getTextDirGui <- function() {
  files <- choose.files()
   #get all files from a directory
  return(getTextFiles(files))
}

corpus.add.attributes <- function(corpus, newattribs, name=newattribs) {
  # attribs should be a named list of length(corpus$texts)
  # can be one or more variables
  newattribs <- as.data.frame(newattribs, stringsAsFactors=FALSE)
  names(newattribs) <- name
  corpus$attribs <- cbind(corpus$attribs, newattribs)
  return(corpus)
}

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

corpus.create <- function(texts, textnames=NULL, attribs=NULL, source=NULL, notes=NULL, attribs.labels=NULL) {
  if (is.null(names(texts))) 
    names(texts) <- paste("text", 1:length(texts), sep="")
  if (is.null(source)) 
    source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
  created <- date()
  metadata <- c(source=source, created=created, notes=notes)
  if (!is.null(attribs)) {
    attribs <- data.frame(texts=texts,
                          attribs,
                          row.names=names(texts), 
                          check.rows=TRUE, stringsAsFactors=FALSE)
  }
  if (!is.null(attribs) & is.null(attribs.labels)) {
    attribs.labels <- c("Original texts", rep(NULL, length(attribs)-1))
  }
  temp.corpus <- list(attribs=attribs,
                      attribs.labels=attribs.labels,
                      metadata=metadata)
  class(temp.corpus) <- list("corpus", class(temp.corpus))
  return(temp.corpus)
}

corpus.append <- function(corpus1, newtexts, newattribs, ...) {
  # function to add new texts and attributes to an existing corpus
  # should make it also allow an optional corpus2 version where two
  # corpuses could be combined with corpus.append(corp1, corp2)
  # if we can verify the same attribute set.
  tempcorpus <- corpus.create(newtexts, attribs=newattribs)
  corpus1$attribs <- rbind(tempcorpus$attribs, corpus1$attribs)
  # TODO: implement concatenation of any attribs.labels from new corpus
  return(corpus1)
}

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
  print(head(cbind((dtexts <- describetexts(texts, output=FALSE)),
                   attribs), 
             nmax))
  cat("\nSource:  ", corpus$metadata["source"], ".\n", sep="")
  cat("Created: ", corpus$metadata["created"], ".\n", sep="")
  cat("Notes:   ", corpus$metadata["notes"], ".\n\n", sep="")
  # invisibly pass the summary of the texts from describetexts()
  return(invisible(dtexts))
}

describetexts <- function(texts, output=TRUE) {
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

tokenize <- function(input.text, input.text.name="count") {
  # returns a dataframe of word counts, word is 1st column
  #
  ## clean up stuff in the text
  clean.txt <- gsub("[[:punct:][:digit:]]", "", input.text)
  # for French, make "l'" into "l"
  input.text <- gsub("l'", "l ", input.text)
  # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
  clean.txt <- gsub("ß", "ss", clean.txt)
  # make all words lowercase
  clean.txt <- tolower(clean.txt)
  # tokenize
  tokenized.txt <- scan(what="char", text=clean.txt, quiet=TRUE)
  # flush out "empty" strings caused by removal of punctuation and numbers
  tokenized.txt <- tokenized.txt[tokenized.txt!=""]
  ## tabulate word counts
  ## and return as a data frame with variables "word" and given name
  wf.list <- as.data.frame(table(tokenized.txt))
  names(wf.list) <- c("feature", input.text.name)
  return(wf.list)
}


create.fvm.corpus <- function(corpus,
                              feature=c("word"),
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
    texts <- split(corpus$attribs$texts, as.factor(corpus$attribs[,groups]))
    texts <- sapply(texts, paste)
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
    if (i/length(texts) > progress.threshold) {
      if (verbose) cat(paste("...", progress.threshold*100, sep=""))
      progress.threshold <- progress.threshold+.1
    }
    temp.fvm <- tokenize(texts[i], textnames[i])
    fvm <- merge(fvm, temp.fvm, by="feature", all=TRUE)
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
