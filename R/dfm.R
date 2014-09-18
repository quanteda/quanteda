#' Create a document-feature matrix from a corpus object 
#'
#' returns a document by feature matrix compatible with austin.  A typical usage would
#' be to produce a word-frequency matrix where the cells are counts of words by document.
#' 
#' @param x Corpus or character vector from which to generate the document-feature matrix
#' @param feature Feature to count (e.g. words)
#' @param stem Stem the words
#' @param stopwords A character vector of stopwords that will be removed from the text when constructing the \link{dfm}.  If \code{NULL} (default)
#' then no stopwords will be applied.  If "TRUE" then it currently defaults to \code{\link{stopwords}}.
#' @param groups Grouping variable for aggregating documents
#' @param verbose Get info to screen on the progress
#' @param dictionary A list of character vector dictionary entries, including regular expressions (see examples) 
#' @param dictionary_regex \code{TRUE} means the dictionary is already in regular expression format,
#' otherwise it will be converted from "wildcard" format
#' @param addto \code{NULL} by default, but if an existing dfm object is specified, then the new dfm will be added to the one named.
#' If both \link{dfm}'s are built from dictionaries, the combined dfm will have its \code{Non_Dictionary} total adjusted.
#' @return A matrix object with row names equal to the document names and column names equal to the feature labels.  
#' This matrix has \code{names(dimnames) = c("docs", "words")}
#' to make it conformable to an \link[austin]{wfm} object.
#' @rdname dfm
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' data(inaugCorpus)
#' wfm <- dfm(inaugCorpus)
#' 
#' ## by president, after 1960
#' wfmByPresfrom1900 <- dfm(subset(inaugCorpus, Year>1900), groups="President")
#' docnames(wfmByPresfrom1900)
#' 
#' ## with dictionaries
#' data(inaugCorpus)
#' mycorpus <- subset(inaugCorpus, Year>1900)
#' mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
#'                opposition=c("Opposition", "reject", "notincorpus"),
#'                taxing="taxing",
#'                taxation="taxation",
#'                taxregex="tax*")
#' dictDfm <- dfm(mycorpus, dictionary=mydict)
#' dictDfm
#' 
#' ## removing stopwords
#' testText <- "The quick brown fox named Seamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."
#' testCorpus <- corpus(testText)
#' settings(testCorpus, "stopwords")
#' dfm(testCorpus, stopwords=TRUE)
#' 
dfm <- function(x, ...) {
#                 feature=c("word"),
#                 stem=FALSE,
#                 stopwords=NULL,
#                 bigram=FALSE,
#                 groups=NULL,
#                 verbose=TRUE, 
#                 dictionary=NULL,
#                 dictionary_regex=FALSE,
#                 clean=TRUE,
#                 removeDigits=TRUE, removePunct=TRUE, lower=TRUE,                          
#                 addto=NULL) {
  UseMethod("dfm")
}

#' @rdname dfm
#' @method dfm corpus
#' @S3method dfm corpus
dfm.corpus <- function(x,
                       feature=c("word"),
                       stem=FALSE,
                       stopwords=NULL,
                       bigram=FALSE,
                       groups=NULL,
                       verbose=TRUE, 
                       dictionary=NULL,
                       dictionary_regex=FALSE,
                       clean=TRUE,
                       removeDigits=TRUE, removePunct=TRUE, lower=TRUE,                          
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
    
    # aggregation by group
    if (!is.null(groups)) {
        if (verbose) cat("aggregating by group: ", groups, "... ", sep="")
        if (length(groups)>1) {
            group.split <- lapply(documents(x)[, groups], as.factor)
        } else group.split <- as.factor(documents(x)[,groups])
        texts <- split(texts(x), group.split)
        texts <- sapply(texts, paste, collapse = " ")
        if (verbose) cat("complete ...")
    } else {
        texts <- texts(x)
        names(texts) <- docnames(x)
    }
    
    # changing verbose to 2 (instead of TRUE) means will not print message twice
    # when the function calls dfm.character
    tempdfm <- dfm(texts, feature=feature, stem=stem, stopwords=stopwords, bigram=bigram, 
                   verbose=2, dictionary=dictionary, dictionary_regex=dictionary_regex, 
                   addto=addto)
    attr(tempdfm, "settings") <- settings(x)
    tempdfm
}

#' @rdname dfm
#' @method dfm character
#' @S3method dfm character
dfm.character <- function(x,
                          feature=c("word"),
                          stem=FALSE,
                          stopwords=NULL,
                          bigram=FALSE,
                          # groups=NULL,
                          verbose=TRUE, 
                          dictionary=NULL,
                          dictionary_regex=FALSE,
                          clean=TRUE,
                          removeDigits=TRUE, removePunct=TRUE, lower=TRUE,                          
                          addto=NULL, ...) {
    # if (verbose & parent.env(dfm.character) != dfm.corpus) cat("Creating dfm: ...")
    if (verbose==TRUE) cat("Creating dfm from character vector ...")
    
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    textnames <- factor(names(x))
    
    # clean options
    if (clean) {
        x <- clean(x, removeDigits, removePunct, lower)
    }
    
    tokenizedTexts <- tokenize(x, clean=FALSE)
    if (stem==TRUE) {
        require(SnowballC, quietly=TRUE)
        if (verbose) cat(" stemming ...")
        tokenizedTexts <- lapply(tokenizedTexts, wordStem)
    }
    if (bigram > 0) {
        if (verbose) cat(" making bigrams ...")
        tokenizedTexts <- lapply(tokenizedTexts, function(x) bigrams(x, bigram))
    }
    
    # get original sort order, so that we can restore original order after table 
    # alphabetizes the documents (rows of the dfm)
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    
    # print(length)
    alltokens <- data.frame(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            words = unlist(tokenizedTexts, use.names=FALSE))
    
    # need to enforce check that dictionary is a named list
    if (is.null(dictionary)) {
        dfm <- as.data.frame.matrix(table(alltokens$docs, alltokens$words))
    } else {
        # flatten the dictionary
        dictionary <- flatten.dictionary(dictionary)
        # convert wildcards to regular expressions (if needed) 
        if (!dictionary_regex) {
            dictionary <- lapply(dictionary, makeRegEx)
        }
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
    
    # re-written PN 30th June
    if (!is.null(stopwords)) {
        if (verbose) cat(" removing stopwords ... ")
        # need two separate checks because if() on a char vector gives warning
        if (!is.character(stopwords)){
            if (stopwords==TRUE){
                stopwords <- stopwordsGet()
            }
        }
        if (!is.character(stopwords) | !length(stopwords)>0) {
            stop("stopwords must be a character vector with positive length.")
        }
        if (bigram==TRUE) {
            pat <- paste(paste0(paste0("-", stopwords, "$"), collapse='|'), paste0(paste0("^", stopwords, "-"), collapse='|'), sep='|')
            dfm <- t(subset(t(dfm), !grepl(pat, colnames(dfm))))
        } else {
            dfm <- t(subset(t(dfm), !colnames(dfm) %in% stopwords))
        }
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
    
    dfm <- dfm[(1:nrow(dfm))[order(originalSortOrder)], ]
    
    if(verbose) cat(" done. \n")
    class(dfm) <- c("dfm", class(dfm))
    return(dfm)
}


#' Flatten a hierarchical dictionary into a list of character vectors
#'
#' Converts a hierarchical dictionary (a named list of named lists, ending in character 
#' vectors at the lowest level) into a flat list of character vectors.  Works like
#' \code{unlist(dictionary, recursive=TRUE)} except that the recursion does not go to the 
#' bottom level.
#' 
#' Called by dfm()
#' 
#' @param elms list to be flattened
#' @param parent parent list name, gets built up through recursion in the same way that \code{unlist(dictionary, recursive=TRUE)} works 
#' @param dict the bottom list of dictionary entries ("synonyms") passed up from recursive calls
#' @return A dictionary flattened down one level further than the one passed
#' @export 
#' @author Kohei Watanabe
#' @examples 
#' dictPopulismEN <- 
#'     list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
#'                     "corrupt*", "propagand", "politici*", "*deceit*",
#'                     "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
#'                     "dishonest*", "establishm*", "ruling*"))
#' flatten.dictionary(dictPopulismEN)
#' 
#' hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#'                              level1a2 = c("l1a21", "l1a22")),
#'               level1b = list(level1b1 = c("l1b11", "l1b12"),
#'                              level1b2 = c("l1b21", "l1b22", "l1b23")),
#'               level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#'                              level1c1b = list(level1c1b1 = c("lowestalone"))))
#' flatten.dictionary(hdict)
flatten.dictionary <- function(elms, parent = '', dict = list()) {
    for (self in names(elms)) {
        elm <- elms[[self]]
        if (parent != '') {
            self <- paste(parent, self, sep='.')
        }
        # print("-------------------")
        # print (paste("Name", self))
        if (is.list(elm)) {
            # print("List:")
            # print(names(elm))
            dict <- flatten.dictionary(elm, self, dict)
        } else {
            # print("Words:")
            dict[[self]] <- elm
            # print(dict)
        }
    }
    return(dict)
}


makeRegEx <- function(wildcardregex) {
    for (i in 1:length(wildcardregex)) {
        lengthWildCard <- nchar(wildcardregex[i])
        # '*' wildcards at both ends, just remove them
        if ((substr(wildcardregex[i], 1, 1)=="*") & substr(wildcardregex[i], lengthWildCard, lengthWildCard)=="*") {
            # '*' wildcards at both ends, just remove them
            wildcardregex[i] <- substr(wildcardregex[i], 2, lengthWildCard-1)
        } else if (substr(wildcardregex[i], 1, 1)=="*") {
            # '*' wildcard only at beginning, remove and add "$" to end
            wildcardregex[i] <- paste(substr(wildcardregex[i], 2, lengthWildCard), "$", sep="")
        } else if (substr(wildcardregex[i], lengthWildCard, lengthWildCard)=="*") {
            # '*' wildcard only at end, remove and add "^" to beginning
            wildcardregex[i] <- paste("^", substr(wildcardregex[i], 1, lengthWildCard-1), sep="")
        } else if (!((substr(wildcardregex[i], 1, 1)=="*") & substr(wildcardregex[i], lengthWildCard, lengthWildCard)=="*")) {
            # change to ^word$ if no * at all, for exact match
            wildcardregex[i] <- paste("^", wildcardregex[i], "$", sep="")
        } else {
            stop("Any wildcards except * and beginning or end of word not yet implemented.")
        }
    }
    return(wildcardregex)
    ##
    ## TO ADD:
    ##   * in the middle of the word
    ##   ? functionality
    ##   [ab] meaning a or b
}

# # @export
# trim <- function(x, ...) {
#     UseMethod("trim")
# }

#' Trim a dfm based on a subset of features and words
#'
#' Returns a document by feature matrix reduced in size based on document and term frequency, and/or subsampling.
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param minCount minimum feature count
#' @param minDoc minimum number of documents in which a feature appears
#' @param sample how many features to retain (based on random selection)
#' @param verbose print messages
#' @return A \link{dfm} object reduced in size.
#' @export 
#' @author Ken Benoit adapted from code by Will Lowe (see \link[austin]{trim})
#' @examples 
#' data(inaugCorpus)
#' dtm <- dfm(inaugCorpus)
#' dim(dtm) 
#' dtmReduced <- trimdfm(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
#' dim(dtmReduced)  
#' dtmSampled <- trimdfm(dtm, sample=200)  # top 200 words
#' dim(dtmSampled)  # 196 x 200 words
trimdfm <- function(x, minCount=5, minDoc=5, sample=NULL, verbose=TRUE) {
    if (!is.dfm(x)) stop("trimdfm should only be used for dfm objects.")
    class_xorig <- class(x)
    mY <- t(x)
    
    rs1 <- which(rowSums(mY) >= minCount)
    if (verbose)
        cat("Words appearing less than", minCount, "times:", (nrow(mY) - length(rs1)), "\n")
    
    rs2 <- which(apply(mY, 1, function(x){ sum(x>0) >= minDoc } ))
    if (verbose)
        cat("Words appearing in fewer than", minDoc, "documents:", (nrow(mY) - length(rs2)), "\n")
    
    tokeep <- intersect(rs1, rs2)
    if (length(tokeep)==0)
        stop("No words left after trimming.")
    
    if (!is.null(sample)) {
        if (sample > length(tokeep))
            warning(paste('Sample size', sample, 'larger than',
                          length(tokeep), "already filtered from", nrow(mY), "so ignoring sampling request"))
        tokeep <- sample(tokeep, min(length(tokeep), sample))
        if (verbose)
            cat("Retaining a random sample of", sample, "words\n")
    }
    trimmeddfm <- t(mY[sort(tokeep),])
    class(trimmeddfm) <- class_xorig
    trimmeddfm
}

#' @export
tfidf <- function(x) {
    UseMethod("tfidf")
}

#' @export
#' @rdname ndoc
ndoc.dfm <- function(x) {
    nrow(x)
}

#' compute the tf-idf weights of a dfm
#'
#' Returns a matrix of tf-idf weights, as a \link{dfm} object
#' 
#' @rdname tfidf
#' @param x document-feature matrix created by \code{\link{dfm}}
#' @param normalize whether to normalize term frequency by document totals
#' @return A dfm matrix object where values are tf-idf weights
#' @export 
#' @author Ken Benoit
#' @examples 
#' data(inaugCorpus)
#' dtm <- dfm(inaugCorpus)
#' dtm[1:10, 100:110]
#' tfidf(dtm)[1:10, 100:110]
#' tfidf(dtm, normalize=FALSE)[1:10, 100:110]
tfidf.dfm <- function(x, normalize = TRUE) {
    idf <- log(ndoc(x)) - log(colSums(x > 0) + 1)
    if (normalize) {
        x <- x/rowSums(x)
        x[is.nan(x)] <- 0
    }
    return(t(t(x) * idf))
}

#' normalizes the term frequencies a dfm
#'
#' Returns a matrix of term weights, as a \link{dfm} object
#' 
#' @param dfm Document-feature matrix created by \code{\link{dfm}}
#' @return A dfm matrix object where values are relative term proportions within the document
# @export 
#' @author Ken Benoit
#' @examples 
#' data(inaugCorpus)
#' dtm <- dfm(inaugCorpus)
#' dtm[1:10, 100:110]
#' tf(dtm)[1:10, 100:110]
tf <- function(x) {
    return(x/rowSums(x))
}


# @export
types <- function(corp) {
    return(unique(unlist(tokenize(corp))))
}


#' @export
features <- function(x, ...) {
    UseMethod("features")
}

#' extract the feature labels from a \link{dfm}
#' 
#' Extract the features from a document-feature matrix, which are stored as the column names
#' of the \link{dfm} object.
#' @aliases features
#' @return Character vector of the features
#' @examples
#' features(dfm(inaugTexts))[1:50]  # first 50 features (alphabetically sorted)
#' @export
features.dfm <- function(x) {
    colnames(x)
}

#' @rdname docnames
#' @examples
#' # 
#' # query the document names of a dfm
#' docnames(dfm(inaugTexts[1:5]))
#' @export
docnames.dfm <- function(x) {
    rownames(x)
}

#' @details \code{is.dfm} returns \code{TRUE} if and only if its argument is a \link{dfm}.
#' @rdname dfm
#' @export
is.dfm <- function(x) {
    "dfm" %in% class(x)
}


#' sort a dfm by one or more margins
#' 
#' Sorts a \link{dfm} by frequency of total features, total features in 
#' documents, or both
#' 
#' @param dfm Document-feature matrix created by \code{\link{dfm}}
#' @param margin which margin to sort on \code{features} to sort by frequency of
#'   features, \code{docs} to sort by total feature counts in documents, and 
#'   \code{both} to sort by both
#' @param decreasing TRUE (default) if sort will be in descending order, 
#'   otherwise sort in increasing order
#' @return A sorted \link{dfm} matrix object
#' @export
#' @author Ken Benoit
#' @examples 
#' dtm <- dfm(inaugCorpus)
#' dtm[1:10, 1:5]
#' dtm <- sort(dtm)
#' sort(dtm)[1:10, 1:5]
#' sort(dtm, TRUE, "both")[1:10, 1:5]  # note that the decreasing=TRUE argument
#'                                     # must be second, because of the order of the
#'                                     # formals in the generic method of sort()
sort.dfm <- function(x, decreasing=TRUE, margin = c("features", "docs", "both")) {
    margin <- match.arg(margin)
    class_xorig <- class(x)
    if (margin=="features") {
        x <- x[, order(colSums(x), decreasing=decreasing)]
    } else if (margin=="docs") {
        x <- x[order(rowSums(x), decreasing=decreasing), ]
    } else if (margin=="both") {
        x <- x[order(rowSums(x), decreasing=decreasing), 
               order(colSums(x), decreasing=decreasing)]
    }
    class(x) <- class_xorig
    return(x)
}

#' list the most frequent features
#' 
#' List the most frequently occuring features.
#' @return A named numeric vector of feature counts, where the names are the feature labels.
#' @export
#' @examples
#' topfeatures(dfm(inaugCorpus))
#' topfeatures(dfm(inaugCorpus, stopwords=TRUE))
#' # least frequent features
#' topfeatures(dfm(inaugCorpus), decreasing=FALSE)
topfeatures <- function(x, n=10, decreasing=TRUE) {
    UseMethod("topfeatures")
}

#' @rdname topfeatures
#' @export
topfeatures.dfm <- function(x, n=10, decreasing=TRUE) {
    if (is.null(n)) n <- ncol(x)
    subdfm <- sort(colSums(x), decreasing)
    subdfm[1:n]
}

#' @S3method print dfm
#' @export
print.dfm <- function(x) {
    class(x) <- "matrix"
    attr(x, "settings") <- NULL
    print(x)
}