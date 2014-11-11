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
#' @rdname dfm
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
#' print(dfm(compoundWords(mytexts, mydict),
#'           thesaurus=lapply(mydict, function(x) gsub("\\s", "_", x))),
#'       show.values=TRUE)
#' # pick up "taxes" with "tax" as a regex
#' print(dfm(compoundWords(mytexts, mydict), thesaurus=list(anytax="tax"), dictionary_regex=TRUE), TRUE)
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
dfm <- function(x, ...) {
    UseMethod("dfm")
}

#' @rdname dfm
#' @method dfm corpus
#' @export
dfm.corpus <- function(x,
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
        tempdfm <- dfm(texts, feature=feature, stem=stem, stopwords=stopwords, bigram=bigram,
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

#' @rdname dfm
#' @method dfm character
#' @export
dfm.character <- function(x,
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
#' @param minTotal minimum total feature threshold to retain a document
#' @param sample how many features to retain (based on random selection)
#' @param keep regular expression specifying which features to keep
#' @param verbose print messages
#' @return A \link{dfm} object reduced in size.
#' @export
#' @author Ken Benoit adapted from code originally by Will Lowe (see \link[austin]{trim})
#' @examples
#' dtm <- dfm(inaugCorpus)
#' dim(dtm)
#' dtmReduced <- trimdfm(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
#' dim(dtmReduced)
#' dtmReduced <- trimdfm(dtm, keep="^nation|^citizen|^union$")
#' topfeatures(dtmReduced, NULL)
#' dtmSampled <- trimdfm(dtm, sample=200)  # top 200 words
#' dim(dtmSampled)  # 196 x 200 words
trimdfm <- function(x, minCount=1, minDoc=1, minTotal=0, sample=NULL, keep=NULL, verbose=TRUE) {
    if (!is.dfm(x)) stop("trimdfm should only be used for dfm objects.")
    class_xorig <- class(x)
    mY <- t(x)

    rs1 <- which(rowSums(mY) >= minCount)
    if (verbose & minCount>1)
        cat("Words appearing less than", minCount, "times:", (nrow(mY) - length(rs1)), "\n")

    rs2 <- which(apply(mY, 1, function(x){ sum(x>0) >= minDoc } ))
    if (verbose & minDoc>1)
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

    # which features to keep
    if (!is.null(keep)) {
        trimmeddfm <- trimmeddfm[, grep(keep, colnames(trimmeddfm))]
    }

    trimmeddfm <- trimmeddfm[rowSums(trimmeddfm) >= minTotal, , drop=FALSE]

    class(trimmeddfm) <- class_xorig
    trimmeddfm
}


#' @export
#' @rdname ndoc
ndoc.dfm <- function(x, ...) {
    nrow(x)
}


#' compute the tf-idf weights of a dfm
#'
#' Returns a matrix of tf-idf weights, as a \link{dfm} object
#'
#' @export
tfidf <- function(x, normalize = TRUE) {
    UseMethod("tfidf")
}

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
    class_xorig <- class(x)
    idf <- log(ndoc(x)) - log(colSums(x > 0) + 1)
    if (normalize) {
        x <- x/rowSums(x)
        x[is.nan(x)] <- 0
    }
    tmp <- t(t(x) * idf)
    class(tmp) <- class_xorig
    tmp
}

#' normalizes the term frequencies a dfm
#'
#' Returns a matrix of term weights, as a \link{dfm} object
#'
#' @param x Document-feature matrix created by \code{\link{dfm}}
#' @return A dfm matrix object where values are relative term proportions within the document
#' @export
#' @author Ken Benoit
#' @examples
#' data(inaugCorpus)
#' dtm <- dfm(inaugCorpus)
#' dtm[1:10, 100:110]
#' tf(dtm)[1:10, 100:110]
tf <- function(x) {
    class_xorig <- class(x)
    tmp <- x/rowSums(x)
    class(tmp) <- class_xorig
    tmp
}


# @export
types <- function(corp) {
    return(unique(unlist(tokenize(corp))))
}


#' @export
features <- function(x) {
    UseMethod("features")
}

#' extract the feature labels from a \link{dfm}
#'
#' Extract the features from a document-feature matrix, which are stored as the column names
#' of the \link{dfm} object.
#' @param x the object (dfm) whose features will be extracted
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

#' @details \code{as.dfm} coerces a matrix to a dfm
#' @rdname dfm
#' @export
as.dfm <- function(x) {
    if (!("matrix" %in% class(x)))
        stop("is.dfm only applicable to matrix(-like) objects.")
    class(x) <- c("dfm", class(x))
    x
}



#' sort a dfm by one or more margins
#'
#' Sorts a \link{dfm} by frequency of total features, total features in
#' documents, or both
#'
#' @param x Document-feature matrix created by \code{\link{dfm}}
#' @param margin which margin to sort on \code{features} to sort by frequency of
#'   features, \code{docs} to sort by total feature counts in documents, and
#'   \code{both} to sort by both
#' @param decreasing TRUE (default) if sort will be in descending order,
#'   otherwise sort in increasing order
#' @param ... additional argumnets passed to base method \code{sort.int}
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
sort.dfm <- function(x, decreasing=TRUE, margin = c("features", "docs", "both"), ...) {
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
#' List the most frequently occuring features in a \link{dfm}
#' @aliases topFeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If TRUE, return the \code{n} most frequent features, if
#'   FALSE, return the \code{n} least frequent features
#' @param ci confidence interval from 0-1.0 for use if dfm is resampled
#' @export
topfeatures <- function(x, n=10, decreasing=TRUE, ci=.95) {
    UseMethod("topfeatures")
}

#' @return A named numeric vector of feature counts, where the names are the feature labels.
#' @examples
#' topfeatures(dfm(inaugCorpus))
#' topfeatures(dfm(inaugCorpus, stopwords=TRUE))
#' # least frequent features
#' topfeatures(dfm(inaugCorpus), decreasing=FALSE)
#' @export
#' @rdname topfeatures
topfeatures.dfm <- function(x, n=10, decreasing=TRUE, ci=.95) {
    if (is.null(n)) n <- ncol(x)
    if (is.resampled(x)) {
        subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
        subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
        return(data.frame(#features=colnames(subdfm),
                          freq=colSums(subdfm[,,1]),
                          cilo=apply(colSums(subdfm), 1, quantile, (1-ci)/2),
                          cihi=apply(colSums(subdfm), 1, quantile, 1-(1-ci)/2)))
    } else {
        subdfm <- sort(colSums(x), decreasing)
        return(subdfm[1:n])
    }
}

#' print a dfm object
#'
#' print method for dfm objects
#' @param x the dfm to be printed
#' @param show.values print the dfm as a matrix or array (if resampled).
#' @param show.settings Print the settings used to create the dfm.  See
#'   \link{settings}.
#' @param ... further arguments passed to or from other methods
#' @export
print.dfm <- function(x, show.values=FALSE, show.settings=FALSE, ...) {
    cat("Document-feature matrix of: ",
        ndoc(x), " document",
        ifelse(ndoc(x)>1, "s, ", ", "),
        dim(x)[2], " feature",
        ifelse(dim(x)[2]>1, "s", ""),
        ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
        ".\n", sep="")
    if (show.settings) {
        cat("Settings: TO BE IMPLEMENTED.")
    }
    if (show.values | (nrow(x)<=20 & ncol(x)<=20)) {
        class(x) <- class(x)[2]
        attr(x, "settings") <- NULL
        print(x)
    }
}

#' @rdname ndoc
#' @export
nfeature <- function(x) {
    UseMethod("nfeature")
}

#' @rdname ndoc
#' @export
nfeature.corpus <- function(x) {
    stop("nfeature not yet implemented for corpus objects.")
}

#' @rdname ndoc
#' @export
#' @examples
#' nfeature(dfm(inaugCorpus))
#' nfeature(trimdfm(dfm(inaugCorpus), minDoc=5, minCount=10))
nfeature.dfm <- function(x) {
    ncol(x)
}


#' Weight the feature frequencies in a dfm by various methods
#' 
#' Returns a document by feature matrix with the feature frequencies weighted 
#' according to one of several common methods. 
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param type The weighting function to aapply to the dfm. One of: 
#' \itemize{ 
#'   \item normTf - Length normalization: dividing the frequency of the feature 
#'   by the length of the document) 
#'   \item tf-idf - Term-frequency * inverse 
#'   document frequency. For a full explanation, see, for example, 
#'   \url{(http://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html)}.
#'    This implementation will not return negative values. 
#'    \item logTf - The 
#'   natural log of the term frequency 
#'   \item maxTf - The term frequency divided 
#'   by the frequency of the most frequent term in the document 
#'   \item ppmi -   Positive Pointwise Mutual Information }
#' @param smooth Apply additivee smoothing to the matrix before weighting. TRUE 
#'   by default, adding 0.5 to counts.
#' @return The original dfm, with values weighted according to type function.
#' @export
#' @author Paul Nulty
#' @examples
#' dtm <- dfm(inaugCorpus)
#' x <- apply(dtm, 1, function(tf) tf/max(tf))
#' topfeatures(dtm)
#' normDtm <- weight(dtm)
#' topfeatures(normDtm)
#' maxTfDtm <- weight(dtm, type="maxTf")
#' topfeatures(maxTfDtm)
#' logTfDtm <- weight(dtm, type="logTf")
#' topfeatures(logTfDtm)
#' tfidfDtm <- weight(dtm, type="tfidf")
#' topfeatures(tfidfDtm)
#' pmiDtm <- weight(dtm+1, type="ppmi")
#' topfeatures(pmiDtm)
#' 
#' # combine these methods for more complex weightings, e.g. as in Section 6.4 of
#' # Introduction to Information Retrieval
#' logTfDtm <- weight(dtm, type="logTf")
#' wfidfDtm <- weight(logTfDtm, type="tfidf")
#' 
#' @references Manning, Christopher D., Prabhakar Raghavan, and Hinrich Schutze.
#'   Introduction to information retrieval. Vol. 1. Cambridge: Cambridge 
#'   university press, 2008.
weight <- function(x, type=c("normTf","maxTf","logTf", "tfidf", "ppmi"), smooth=FALSE){
    attr_orig <- attributes(x)
    type <- match.arg(type)
    if(smooth){
        x <- smooth(x)
    }
    if(type=="normTf"){
        x <- x/rowSums(x)
        x[is.nan(x)] <- 0
    }else if(type=="maxTf"){
        x <- t(apply(dtm, 1, function(tef) tef/max(tef)))
    }else if(type=="logTf"){
        x <- 1+log(x)
        x[is.nan(x)] <- 0
    }else if(type=="tfidf"){
        idf <- log(ndoc(x)+1) - log(colSums(x > 0.5) + 1)
        x <- t(t(x) * idf)
    } else if (type=="ppmi"){
        pij <- x/rowSums(x)
        pij[is.nan(pij)] <- 0
        pi <- colSums(x)
        pj <- rowSums(x)
        pj[is.nan(pj)] <- 0
        pmi <- (pij / t(outer(pi,pj)))
        x <- abs(pmi)
    } else {
        warning( sprintf("Type %s not implmented, no weighting performed.", type))
    }
    attributes(x) <- attr_orig
    return(x)
}


#' Additive smoothing of feature frequencies in a dfm
#' 
#' Smooths the feature counts by adding a small value (default 0.5) to remove
#' zero counts. Zero counts are problematic for probability-based models.
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param alpha The value to add to all counts. Default is 0.5
#' @return The original dfm, with values weighted according to type function.
#' @export
#' @author Paul Nulty
#' @examples
#' dtm <- dfm(inaugCorpus)
#' dtm[1:5,1:10]
#' smDtm <- smoothdfm(dtm)
#' smDtm[1:5,1:10]
smoothdfm <- function(x, alpha=0.5) {
    attr_orig <- attributes(x)
    x <- x + alpha
    attributes(x) <- attr_orig
    x
}

# [] method for dfm
#' @export
`[.dfm` <- function(x, i, j, ..., drop) {
    m <- NextMethod("[")
    attr(m, "settings") <- attr(x, "settings")
    class(m) <- class(x)
    m
}
