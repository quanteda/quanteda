create.fvm.corpus <-
function(corpus,
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
