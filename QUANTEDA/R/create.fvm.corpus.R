create.fvm.corpus <-
function(corpus,
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
