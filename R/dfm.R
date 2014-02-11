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
#' @return A data frame with row names equal to the document names and column names equal to the feature labels.
#' @rdname dfm
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' \dontrun{
#' data(iebudgets)
#' wfm <- dfm(iebudgets)
#' wfmByParty2010 <- dfm(iebudgets, groups="party", subset=(year==2010))
#' }
dfm <- function(corpus,
                feature=c("word"),
                stem=FALSE,
                stopwords=FALSE,
                groups=NULL,
                subset=NULL, 
                verbose=TRUE) {
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
                       verbose=TRUE) {
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
    dfm <- as.data.frame.matrix(table(alltokens$docs, alltokens$words))

    if(verbose) cat(" done. \n")

    if (stopwords) {
        data(stopwords_EN)
        dfm <- as.wfm(subset(dfm, !row.names(dfm) %in% stopwords_EN))
    }
    return(dfm)
}


