#' Create a document-feature matrix from a corpus object
#'
#' returns a document by feature matrix compatible with austin.  A typical usage would
#' be to produce a word-frequency matrix where the cells are counts of words by document.
#' 
#' @param corpus Corpus from which to generate the document-feature matrix
#' @param feature Feature to count (e.g. words)
#' @param stem Stem the words
#' @param stopwords Remove stopwords
#' @param groups Grouping variable for aggregating documents
#' @param subset Expression for subsetting the corpus before processing
#' @param verbose Get info to screen on the progress
#' @param dictionary A list of character vector dictionary entries, including regular expressions (see examples) 
#' @return A data frame with row names equal to the document names and column names equal to the feature labels.
#' @rdname dfm
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' data(iebudgets)
#' wfm <- dfm(iebudgets)
#' wfmByParty2010 <- dfm(iebudgets, groups="party", subset=(year==2010))
#' 
#' ## with dictionaries
#' corpus <- subset(iebudgets, year==2010)
#' mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
#'                opposition=c("Opposition", "reject", "notincorpus"),
#'                taxing="taxing",
#'                taxation="taxation",
#'                taxregex="tax*")
#' dictDfm <- dfm(corpus, dictionary=mydict)
dfm <- function(corpus,
                feature=c("word"),
                stem=FALSE,
                stopwords=FALSE,
                groups=NULL,
                subset=NULL, 
                verbose=TRUE, 
                dictionary=NULL) {
    UseMethod("dfm")
}

#' @rdname dfm
#' @method dfm corpus
#' @S3method dfm corpus
dfm.corpus <- function(corpus,
                       feature=c("word"),
                       stem=FALSE,
                       stopwords=FALSE,
                       groups=NULL,
                       subset=NULL, 
                       verbose=TRUE, 
                       dictionary=NULL) {
    if (verbose) cat("Creating dfm: ...")

    # subsets 
    if (!is.null(subset)) corpus <- corpus.subset.inner(corpus, substitute(subset))

    # aggregation by group
    if (!is.null(groups)) {
        if (verbose) cat(" aggregating by group: ", groups, "...", sep="")
        if (length(groups)>1) {
            group.split <- lapply(corpus$attribs[,groups], as.factor)
        } else group.split <- as.factor(corpus$attribs[,groups])
        texts <- split(corpus$attribs$texts, group.split)
        # was sapply, changing to lapply seems to fix 2 class case
        texts <- lapply(texts, paste)
        if (verbose) cat("complete...")
    } else {
        texts <- corpus$attribs$texts
        names(texts) <- rownames(corpus$attribs)
    }

    textnames <- factor(names(texts))
    tokenizedTexts <- sapply(texts, tokenize, simplify=FALSE)
    if (stem==TRUE) {
        require(SnowballC)
        tokenizedTexts <- wordStem(tokenizedTexts)
    }
    # print(length)
    alltokens <- data.frame(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            words = unlist(tokenizedTexts, use.names=FALSE))
    
    # need to enforce check that dictionary is a named list
    if (is.null(dictionary)) {
      dfm <- as.data.frame.matrix(table(alltokens$docs, alltokens$words))
    } else {
      alltokens <- cbind(alltokens, 
                         matrix(0, nrow=nrow(alltokens), 
                                ncol=length(names(dictionary)), 
                                dimnames=list(NULL, names(dictionary))))
      #      alltokens$dictionaryWord <- "other"
      for (i in 1:length(dictionary)) {
        dictionary_word_index <- grep(paste(tolower(dictionary[[i]]), collapse="|"), 
                                      alltokens$words)
        alltokens[dictionary_word_index, 2+i] <- 1
      }
      alltokens$All_Words <- 1
      dictsplit <- split(alltokens[, 3:ncol(alltokens)], alltokens$docs)
      dictsum <- sapply(dictsplit, colSums)
      dfm <- as.data.frame.matrix(t(dictsum))
      # doing it this way avoids an error using rowSums if only one dictionary column
      dfm$Non_Dictionary <- 2*dfm$All_Words - rowSums(dfm)
      dfm <- dfm[, -(ncol(dfm)-1)]
    }

    if(verbose) cat(" done. \n")

    if (stopwords) {
        data(stopwords_EN)
        dfm <- as.wfm(subset(dfm, !row.names(dfm) %in% stopwords_EN))
    }
    return(dfm)
}






