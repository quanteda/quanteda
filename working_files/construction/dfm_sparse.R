#' @rdname dfmOld
#' @method dfm character
#' @export
dfmOld.character <- function(x,
                             feature=c("word"),
                             stem=FALSE,
                             stopwords=NULL,
                             bigram=FALSE,
                             # groups=NULL,
                             verbose=TRUE,
                             dictionary=NULL,
                             thesaurus=NULL,
                             dictionary_regex=FALSE,
                             keep=NULL,
                             # clean=TRUE,
                             #removeDigits=TRUE, removePunct=TRUE, lower=TRUE,
                             addto=NULL, ...) {
    # if (verbose & parent.env(dfm.character) != dfm.corpus) cat("Creating dfm: ...")
    if (verbose==TRUE) cat("Creating dfm from character vector ...")
    
    ##
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    ##
    textnames <- factor(names(x))
    
    # clean options
    #x <- clean(x, ...)
    
    # returns a list of tokens = in length to ndoc x replicates
    # the ... are the clean options
    tokenizedTexts <- tokenize(x, ...)
    
    if (!is.null(stopwords)) {
        if (verbose) cat(" removing stopwords ... ")
        # need two separate checks because if() on a char vector gives warning
        if (!is.character(stopwords)) {
            if (stopwords==TRUE) {
                stopwords <- stopwordsGet()
            }
        }
        if (!is.character(stopwords) | !length(stopwords)>0) {
            stop("stopwords must be a character vector with positive length.")
        }
        if (bigram==TRUE) {
            stop("bigram and stopwords not yet implemented.")
            #pat <- paste(paste0(paste0("-", stopwords, "$"), collapse='|'), paste0(paste0("^", stopwords, "-"), collapse='|'), sep='|')
            #dfm <- t(subset(t(dfm), !grepl(pat, colnames(dfm))))
        } else {
            #dfm <- t(subset(t(dfm), !colnames(dfm) %in% stopwords))
            tokenizedTexts <- lapply(tokenizedTexts, stopwordsRemove, stopwords)
        }
    }
    
    if (stem==TRUE) {
        # require(SnowballC, quietly=TRUE)
        if (verbose) cat(" stemming ...")
        tokenizedTexts <- lapply(tokenizedTexts, wordstem)
    }
    if (bigram > 0) {
        if (verbose) cat(" making bigrams ...")
        tokenizedTexts <- lapply(tokenizedTexts, function(x) bigrams(x, bigram))
    }
    
    # get original sort order, so that we can restore original order after table
    # alphabetizes the documents (rows of the dfm)
    ##
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    
    # print(length)
    alltokens <- data.frame(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names=FALSE))
    
    # if keep is supplied as a regex, then keep only those features
    if (!is.null(keep)) {
        alltokens <- alltokens[grep(keep, alltokens$features), ]
        alltokens$features <- factor(alltokens$features) # refactor
    }
    
    # thesaurus to make word equivalencies
    if (!is.null(thesaurus)) {
        thesaurus <- flatten.dictionary(thesaurus)
        if (!dictionary_regex)
            thesaurus <- lapply(thesaurus, makeRegEx)
        for (l in names(thesaurus)) {
            # add the level to the factor, make the label uppercase
            levels(alltokens$features) <- c(levels(alltokens$features), toupper(l))
            alltokens$features[grep(paste(tolower(thesaurus[[l]]), collapse="|"), alltokens$features)] <- toupper(l)
            # remove the assigned levels from the factor
            levels(alltokens$features) <- factor(alltokens$features)
        }
    }
    
    # need to enforce check that dictionary is a named list
    if (is.null(dictionary)) {
        dfm <- as.data.frame.matrix(table(alltokens$docs, alltokens$features))
    } else {
        # flatten the dictionary
        dictionary <- flatten.dictionary(dictionary)
        # convert wildcards to regular expressions (if needed)
        if (!dictionary_regex)
            dictionary <- lapply(dictionary, makeRegEx)
        alltokens <- cbind(alltokens,
                           matrix(0, nrow=nrow(alltokens),
                                  ncol=length(names(dictionary)),
                                  dimnames=list(NULL, names(dictionary))))
        #      alltokens$dictionaryWord <- "other"
        for (i in 1:length(dictionary)) {
            dictionary_word_index <- grep(paste(tolower(dictionary[[i]]), collapse="|"),
                                          alltokens$features)
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
    
    
    if (!is.null(addto)) {
        if (sum(rownames(dfm) != rownames(addto)) > 0) {
            stop("Cannot add to dfm: different document set.")
        }
        addIndex <- which(!(colnames(addto) %in% colnames(dfm)))
        # adjust the "Non_Dictionary" count for the combined object if both are dictionary-based
        if ("Non_Dictionary" %in% colnames(addto) & "Non_Dictionary" %in% colnames(dfm)) {
            dfm[, "Non_Dictionary"] <- addto[, "Non_Dictionary"] - rowSums(as.matrix(dfm[, -ncol(dfm)]))
        }
        dfm <- cbind(addto[, addIndex], dfm)
    }
    
    # give the matrix austin a "wfm"-like record of which margin is features, which is docs
    # for dfm objects, docs are always rows
    dfm <- as.matrix(dfm)
    dimnames(dfm) <- list(docs = rownames(dfm), features = colnames(dfm))
    
    # restore original sort order
    
    dfm <- dfm[(1:nrow(dfm))[order(originalSortOrder)], , drop=FALSE]
    
    if (verbose==TRUE) cat(" done. \n")
    class(dfm) <- c("dfm", class(dfm))
    return(dfm)
}



#' Create a document-feature matrix from a corpus object
#'
#' Returns a document by feature matrix with additional meta-information
#' (settings, identification of training texts for supervised models, resampling
#' information, etc.) that is useful in other quanteda functions.  A typical
#' usage would be to produce a word-frequency matrix where the cells are counts
#' of words by document, but the definition of "features" is entirely general.
#' @param x Corpus or character vector from which to generate the
#'   document-feature matrix
#' @param feature Feature to count (e.g. words)
#' @param stem Stem the words
#' @param stopwords A character vector of stopwords that will be removed from
#'   the text when constructing the \link{dfm}.  If \code{NULL} (default) then
#'   no stopwords will be applied.  If "TRUE" then it currently defaults to
#'   \code{\link{stopwords}}.
#' @param groups Grouping variable for aggregating documents
#' @param verbose Get info to screen on the progress
#' @param dictionary A list of character vector dictionary entries, including
#'   regular expressions (see examples)
#' @param thesaurus A list of character vector "thesaurus" entries, in a
#'   dictionary list format, which can also include regular expressions  if
#'   \code{dictionary_regex} is \code{TRUE} (see examples).  Note that unlike
#'   dictionaries, each entry in a thesaurus key must be unique, otherwise only
#'   the first match in the list will be used.  Thesaurus keys are converted to
#'   upper case to create a feature label in the dfm, as a reminder that this
#'   was not a type found in the text, but rather the label of a thesaurus key.
#' @param dictionary_regex \code{TRUE} means the dictionary is already in
#'   regular expression format, otherwise it will be converted from "wildcard"
#'   format
#' @param keep a regular expression specifying which features to keep
#' @param bigram include bigrams as well as unigram features, if \code{TRUE}
#' @param addto \code{NULL} by default, but if an existing dfm object is
#'   specified, then the new dfm will be added to the one named. If both
#'   \link{dfm}'s are built from dictionaries, the combined dfm will have its
#'   \code{Non_Dictionary} total adjusted.
#' @param bootstrap if \code{TRUE}, compute multiple \code{dfm}'s from resampled
#'   texts in the corpus.  Requires a resampled corpus.  See
#'   \code{\link{resample}}.
#' @param ... additional arguments passed to \code{\link{clean}}
#' @return A specially classed matrix object with row names equal to the
#'   document names and column names equal to the feature labels.  Additional
#'   information is attached to this object as \code{\link{attributes}}, such as
#'   \link{settings}.
#' @rdname dfmOld
#' @export
#' @author Kenneth Benoit
#' @examples
#' wfm <- dfm(inaugCorpus)
#'
#' ## by president, after 1960
#' wfmByPresfrom1900 <- dfm(subset(inaugCorpus, Year>1900), groups="President")
#' docnames(wfmByPresfrom1900)
#'
#' ## with dictionaries
#' mycorpus <- subset(inaugCorpus, Year>1900)
#' mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
#'                opposition=c("Opposition", "reject", "notincorpus"),
#'                taxing="taxing",
#'                taxation="taxation",
#'                taxregex="tax*",
#'                country="united states")
#' dictDfm <- dfm(mycorpus, dictionary=mydict)
#' print(dictDfm, show.values=TRUE)
#'
#' ## with the thesaurus feature
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised a taxes: an income tax and a sales tax.")
#' mydict <- list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax"))
#' dfm(compoundWords(mytexts, mydict), thesaurus=lapply(mydict, function(x) gsub("\\s", "_", x)))
#' # pick up "taxes" with "tax" as a regex
#' dfm(compoundWords(mytexts, mydict), thesaurus=list(anytax="tax"), dictionary_regex=TRUE)
#'
#' ## removing stopwords
#' testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
#'              the newspaper from a a boy named Seamus, in his mouth."
#' testCorpus <- corpus(testText)
#' settings(testCorpus, "stopwords")
#' print(dfm(testCorpus, stopwords=TRUE), TRUE)
#'
#' ## keep only certain words
#' print(dfm(testCorpus, keep="s$"), TRUE)  # keep only words ending in "s"
#' testTweets <- c("My homie @@justinbieber #justinbieber getting his shopping on in #LA yesterday #beliebers",
#'                 "To all the haters including my brother #justinbieber #justinbiebermeetcrystaltalley #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #kidrauhl #fetusjustin #EMABiggestFansJustinBieber")
#' print(dfm(testTweets, keep="^#"), TRUE)  # keep only hashtags
dfmOld <- function(x, ...) {
    UseMethod("dfmOld")
}

#' @rdname dfmOld
#' @method dfm corpus
#' @export
dfmOld.corpus <- function(x,
                          feature=c("word"),
                          stem=FALSE,
                          stopwords=NULL,
                          bigram=FALSE,
                          groups=NULL,
                          verbose=TRUE,
                          dictionary=NULL,
                          thesaurus=NULL,
                          dictionary_regex=FALSE,
                          keep=NULL,
                          bootstrap=FALSE,
                          # clean=TRUE,
                          # removeDigits=TRUE, removePunct=TRUE, lower=TRUE,
                          addto=NULL, ...) {
    
    # use corpus settings unless overrridden by call
    # settingsGet(x, as.list(match.call()))
    
    if (!is.null(x$tokens)) {
        if (verbose) cat("Using dfm found in corpus.")
        return(x$tokens$dfm)
    }
    
    if (verbose) cat("Creating dfm from a corpus: ... ")
    
    # subsets
    #if (!is.null(subset)) x <- corpus.subset.inner(x, substitute(subset))
    
    if (!is.null(groups))
        if (verbose) cat("aggregating by group: ", groups, "... ", sep="")
    
    nreps <- 1
    if (bootstrap) {
        if (!is.resampled(x)) stop("cannot bootstrap if corpus is not resampled.")
        if (verbose) cat("using", nresample(x), "resamples ... ")
        nreps <- nresample(x) + 1
    }
    
    curtexts <- texts(x)
    for (i in 1:nreps) {
        if (i>1) curtexts <- x$documents[, paste("_resample", i-1, sep="")]
        
        # aggregation by group
        if (!is.null(groups)) {
            if (length(groups)>1) {
                group.split <- lapply(documents(x)[, groups], as.factor)
            } else group.split <- as.factor(documents(x)[,groups])
            texts <- split(curtexts, group.split)
            texts <- sapply(texts, paste, collapse = " ")
            if (verbose) cat("complete ...")
        } else {
            texts <- curtexts
            names(texts) <- docnames(x)
        }
        
        # changing verbose to 2 (instead of TRUE) means will not print message twice
        # when the function calls dfm.character
        tempdfm <- dfmOld(texts, feature=feature, stem=stem, stopwords=stopwords, bigram=bigram,
                       verbose=ifelse(verbose==TRUE, 2, FALSE),
                       dictionary=dictionary, thesaurus=thesaurus,
                       dictionary_regex=dictionary_regex,
                       keep=keep,
                       addto=addto, ...)
        
        if (nreps==1)
            resultdfm <- tempdfm
        
        if (nreps>1) {
            if (i == 1) {
                # initialize the array
                resultdfm <- array(tempdfm, dim=c(dim(tempdfm), nreps))
            } else {
                # add to the array if not the first rep
                cat("\n", dim(resultdfm),
                    "\n", dim(tempdfm))
                resultdfm[,,i] <- tempdfm
            }
        }
    }
    
    # add settings as an attribute
    attr(resultdfm, "settings") <- settings(x)
    # class and label dimnames if an array
    if (length(dim(resultdfm)) > 2) {
        dimnames(resultdfm) <- list(docs = rownames(tempdfm),
                                    features = colnames(tempdfm),
                                    resample = 0:(nreps-1))
        class(resultdfm) <- c("dfm", class(resultdfm))
    }
    
    if (verbose) cat("done.\n")
    resultdfm
}



##########
#########



tokenizeSingle <- function(s, sep=" ", useclean=FALSE, ...) {
    if (useclean) s <- clean(s, ...)
    # s <- unlist(s)
    tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
    return(tokens)
}
