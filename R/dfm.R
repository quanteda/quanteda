#' create a document-feature matrix
#' 
#' Create a dense or sparse matrix dfm from a corpus or a vector of texts.  The sparse
#' matrix construction uses  the
#' \pkg{Matrix} package, and is both much faster and much more memory efficient
#' than the dense form of the \link{dfm} object. 
#' 
#' Eventually the plan is to represent all dfm's as sparse matrixes, but for now the default is
#' to create a dense matrix (\code{matrixType = "dense"}).
#' @param x corpus or character vector from which to generate the document-feature matrix
#' @param ... additional arguments passed to \code{\link{clean}}
#' @import Matrix
#' @export
dfm <- function(x, ...) {
    UseMethod("dfm")
}

#' @rdname dfm
#' @param verbose display messages if \code{TRUE}
#' @param clean if \code{FALSE}, do no cleaning of the text
#' @param stem if \code{TRUE}, stem words
#' @param ignoredFeatures a character vector of user-supplied features to
#'   ignore, such as "stop words".  Formerly, this was a Boolean option for
#'   \code{stopwords = TRUE}, but requiring the user to supply the list
#'   highlights the choice involved in using any stopword list.  To access one
#'   possible list (from any list you wish), use the
#'   \code{\link{stopwordsGet}()} function or just (e.g.)
#'   \code{stopwords$english}.
#' @param keptFeatures a use supplied regular expression defining which features to
#'   keep, while excluding all others.  This can 
#' be used in lieu of a dictionary if there are only specific features that
#' a user wishes to keep.  To extract only Twitter user names hashtags, set
#' \code{keep = "@@\\w+\\b"} and make sure that \code{twitter = TRUE} as an additional
#' argument passed to \link{clean}.
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
#' @param bigrams include bigrams as well as unigram features, if \code{TRUE}
#' @param addto \code{NULL} by default, but if an existing dfm object is
#'   specified, then the new dfm will be added to the one named. If both
#'   \link{dfm}'s are built from dictionaries, the combined dfm will have its
#'   \code{Non_Dictionary} total adjusted.
#' @param language Language for stemming and stopwords.  Choices are 
#'   \code{danish, dutch, english, finnish, french, german, hungarian, italian, 
#'   norwegian, porter, portuguese, romanian, russian, spanish, swedish, 
#'   turkish} for stemming, and \code{SMART, danish, english, french, hungarian,
#'   norwegian, russian, swedish, catalan, dutch, finnish, german, italian, 
#'   portuguese, spanish, arabic} for stopwords.
#' @param matrixType if \code{dense}, produce a dense matrix; or it \code{sparse} produce a 
#'   sparse matrix of class \code{dgCMatrix} from the \pkg{\link{Matrix}} 
#'   package.
#'   @param fromCorpus a system flag used internally, soon to be phased out.
#' @return A specially classed \link[Matrix]{Matrix} object with row names equal
#'   to the document names and column names equal to the feature labels.
#' @author Kenneth Benoit
#' @export
#' @examples
#' # with inaugural texts
#' (size1 <- object.size(dfm(inaugTexts, matrixType="sparse")))
#' (size2 <- object.size(dfm(inaugTexts, matrixType="dense")))
#' cat("Compacted by ", round(as.numeric((1-size1/size2)*100), 1), "%.\n", sep="")
#' 
#' # with stopwords English, stemming, and dense matrix
#' dfmsInaug2 <- dfm(inaugCorpus, ignoredFeatures = stopwordsGet(), stem=TRUE, matrixType="dense")
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
#' dictDfm
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
#' dfm(testCorpus, stopwords=TRUE)
#'
#' ## keep only certain words
#' dfm(testCorpus, keep="s$")  # keep only words ending in "s"
#' testTweets <- c("My homie @@justinbieber #justinbieber getting his shopping on in #LA yesterday #beliebers",
#'                 "To all the haters including my brother #justinbieber #justinbiebermeetcrystaltalley #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #kidrauhl #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(testTweets, keep="^#")  # keep only hashtags
#' 
#' 
#' \dontrun{
#' # try it with approx 35,000 court documents from Lauderdale and Clark (200?)
#' load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
#' txts <- unlist(Opinion_files[1])
#' names(txts) <- NULL
#' 
#' # dfms without cleaning
#' require(Matrix)
#' system.time(dfmsBig <- dfm(txts, clean=FALSE, verbose=FALSE))
#' object.size(dfmsBig)
#' dim(dfmsBig)
#' # compare with tm
#' require(tm)
#' tmcorp <- VCorpus(VectorSource(txts))
#' system.time(tmDTM <- DocumentTermMatrix(tmcorp))
#' object.size(tmDTM)
#' dim(tmDTM)
#'  
#' # with cleaning - the gsub() calls in clean() take a long time
#' system.time(dfmsBig <- dfm(txts, clean=TRUE, additional="[-_\\x{h2014}]")) 
#' object.size(dfmsBig)
#' dim(dfmsBig) 
#' # 100 top features
#' topf <- colSums(dfmsBig)
#' names(topf) <- colnames(dfmsBig)
#' head(sort(topf, decreasing=TRUE), 100)
#' }
dfm.character <- function(x, verbose=TRUE, clean=TRUE, stem=FALSE, 
                           ignoredFeatures = NULL, keptFeatures=NULL,
                           matrixType=c("dense", "sparse"), 
                           language="english",
                           fromCorpus=FALSE, bigrams=FALSE, 
                           thesaurus=NULL, dictionary=NULL, dictionary_regex=FALSE, 
                           addto=NULL, 
                           ...) {
    startTime <- proc.time()
    matrixType <- match.arg(matrixType)
    if (!fromCorpus & verbose) 
        cat("Creating a dfm from a character vector ...")
    
    if (verbose) cat("\n   ... indexing ", 
                     format(length(x), big.mark=","), " document",
                     ifelse(length(x) > 1, "s", ""), sep="")
    docIndex <- 1:length(x)
    if (is.null(names(x))) 
        names(docIndex) <- factor(paste("text", 1:length(x), sep="")) else
            names(docIndex) <- names(x)
    
    if (verbose) cat("\n   ... tokenizing texts")
    if (!bigrams) {
        tokenizedTexts <- lapply(x, tokenizeSingle, sep=" ")
    } else {
        tokenizedTexts <- bigrams(x, include.unigrams=TRUE)
        if (verbose) cat (" (and forming bigrams)")
    }
    
    #if (verbose) cat("\n   ... shaping tokens into data.table")
    alltokens <- data.table(docIndex = rep(docIndex, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts))
    alltokens <- alltokens[features != ""]
    if (verbose) cat(", found", format(nrow(alltokens), big.mark=","), "total tokens")
    if (verbose & bigrams) 
        cat(" incl.", format(sum(grepl("_", alltokens$features)), big.mark=","), "bigrams")
    
    if (clean) {
        if (verbose) cat("\n   ... cleaning the tokens")
        alltokens$features <- clean(alltokens$features, ...)
        if (verbose) cat(", ", nrow(alltokens[features == ""]), " removed entirely", sep="")
    }
    ## commented out to keep words that get removed in cleaning 
    # alltokens <- alltokens[features != ""]
    
    if (stem == TRUE) {
        # require(SnowballC, quietly=TRUE)
        language <- tolower(language)
        if (!(language %in% SnowballC::getStemLanguages())) {
            cat("\n   ... WARNING: not stemming because language", language, "is unavailable")
        } else {
            if (verbose) cat("\n   ... stemming the tokens (", language, ")", sep="")
            alltokens$features <- wordstem(alltokens$features, language=language)
        }
    }
    
    if (!is.null(ignoredFeatures)) {
        if (!is.character(ignoredFeatures)) {
            cat("\n   ... WARNING: not ignoring words because not a character vector")
        } else {
            if (verbose) cat("\n   ... ignoring", format(length(ignoredFeatures), big.mark=","), "feature types, discarding ")
            ignoredfeatIndex <- which(alltokens$features %in% ignoredFeatures)
            if (verbose) {
                cat(format(length(ignoredfeatIndex), big.mark=","), " total features (",
                    format(length(ignoredfeatIndex) / nrow(alltokens) * 100, digits=3),
                    "%)", sep="")
            }
            alltokens <- alltokens[!ignoredfeatIndex]
        }
    }
    
    # thesaurus to make word equivalencies
    if (!is.null(thesaurus)) {
        thesaurus <- flatten.dictionary(thesaurus)
        if (!dictionary_regex)
            thesaurus <- lapply(thesaurus, makeRegEx)
        for (l in names(thesaurus)) 
            alltokens$features[grep(paste(tolower(thesaurus[[l]]), collapse="|"), alltokens$features)] <- toupper(l)
    }
    
    # if keep is supplied as a regex, then keep only those features
    if (!is.null(keptFeatures)) {
        alltokens <- alltokens[grep(keptFeatures, alltokens$features), ]
    }
    
    # dictionary function to select only dictionary terms
    if (!is.null(dictionary)) {
        if (verbose) cat("\n   ... applying a dictionary ")
        # NEED SOME ERROR CHECKING HERE
        # flatten the dictionary
        dictionary <- flatten.dictionary(dictionary)
        if (verbose) cat("consisting of ", length(dictionary), " key entr", 
                         ifelse(length(dictionary) > 1, "ies", "y"), sep="")
        # convert wildcards to regular expressions (if needed)
        if (!dictionary_regex)
            dictionary <- lapply(dictionary, makeRegEx)
        # append the dictionary keys to the table
        alltokens <- cbind(alltokens,
                           matrix(0, nrow=nrow(alltokens),
                                  ncol=length(names(dictionary)),
                                  dimnames=list(NULL, names(dictionary))))
        #      alltokens$dictionaryWord <- "other"
        # loop through dictionary keys and entries and increment counters
        for (i in 1:length(dictionary)) {
            dictionary_word_index <- grep(paste(tolower(dictionary[[i]]), collapse="|"),
                                          alltokens$features)
            alltokens[dictionary_word_index, 2+i] <- 1
        }
        # condition is to handle "null string" features (removed entirely in clean step)
        alltokens$All_Words <- ifelse(alltokens$features != "", 1, 0)
        dictsplit <- split(alltokens[, 3:ncol(alltokens), with=FALSE], alltokens$docIndex)
        dictsum <- sapply(dictsplit, colSums)
        dfmresult <- as.data.frame.matrix(t(dictsum))
        dimnames(dfmresult) <- list(docs=names(docIndex), features=colnames(dfmresult))
        # doing it this way avoids an error using rowSums if only one dictionary column
        dfmresult$Non_Dictionary <- 2*dfmresult$All_Words - rowSums(dfmresult)
        dfmresult <- dfmresult[, -(ncol(dfmresult)-1)]
        
        # convert to a sparse matrix
        dfmresult <- Matrix(as.matrix(dfmresult), sparse=TRUE)
        
    } else {
        
        if (verbose) cat("\n   ... summing tokens by document")
        alltokens[, n:=1L]
        alltokens <- alltokens[, by=list(docIndex,features), sum(n)]
        
        if (verbose) cat("\n   ... indexing ")
        uniqueFeatures <- unique(unlist(alltokens$features))
        uniqueFeatures <- sort(uniqueFeatures)
        # are any features the null string?
        blankFeatureIndex <- which(uniqueFeatures == "")
        totalfeatures <- length(uniqueFeatures) - (length(blankFeatureIndex) > 0)
        if (verbose) cat(format(totalfeatures, big.mark=","), " feature",
                         ifelse(totalfeatures > 1, "s", ""), sep="")
        # much faster than using factor(alltokens$features, levels=uniqueFeatures) !!
        featureTable <- data.table(featureIndex = 1:length(uniqueFeatures),
                                   features = uniqueFeatures)
        setkey(alltokens, features)
        setkey(featureTable, features)
        # merge, data.table style.  warnings suppressed or it moans about mixed encodings
        suppressWarnings(alltokens <- alltokens[featureTable])
        
        if (verbose) cat("\n   ... building sparse matrix")
        suppressWarnings(dfmresult <- sparseMatrix(i = alltokens$docIndex, 
                                                   j = alltokens$featureIndex, 
                                                   x = alltokens$V1, 
                                                   dimnames=list(docs=names(docIndex), features=uniqueFeatures)))
        # zero out "" counts for documents that count other features, meaning that
        # only documents with NO OTHER features than null "" (because of cleaning)
        # will have a positive count for the "" field.  To count "" from cleaning,
        # just comment this next command out
        #
        # the reason to record a positive count for documents whose only 
        # feature is a null feature is that sparse matrixes cannot be all zero
        # blankFeatureIndex <- which(uniqueFeatures == "")
        # dfmresult[which(rowSums(dfmresult[, -blankFeatureIndex]) > 0), ""] <- 0
        
        # different approach: remove null strings entirely
        dfmresult <- dfmresult[, -blankFeatureIndex]
    }
    # class(dfmsparse) <- c("dfms", class(dfmsparse))
    # NEED ANOTHER CLASS METHODS SINCE THIS IS S4
    
    if (!is.null(addto)) {
        if (sum(rownames(dfmresult) != rownames(addto)) > 0) {
            stop("Cannot add to dfm: different document set.")
        }
        addIndex <- which(!(colnames(addto) %in% colnames(dfm)))
        # adjust the "Non_Dictionary" count for the combined object if both are dictionary-based
        if ("Non_Dictionary" %in% colnames(addto) & "Non_Dictionary" %in% colnames(dfmresult)) {
            dfm[, "Non_Dictionary"] <- addto[, "Non_Dictionary"] - rowSums(as.matrix(dfmresult[, -ncol(dfmresult)]))
        }
        dfmresult <- cbind(addto[, addIndex], dfmresult)
    }
    
    # add settings as an attribute
    # attr(resultdfm, "settings") <- settings(x)
    # class and label dimnames if an array
    if (matrixType == "dense") {
        if (verbose) cat("\n   ... converting to a dense matrix")
        dfmresult  <- as.matrix(dfmresult)
        class(dfmresult) <- c("dfm", class(dfmresult))
    }
    
    if (verbose) cat("\n   ... done: created a", paste(dim(dfmresult), collapse=" x "), 
                     ifelse(matrixType=="dense", "dense", "sparse"), "dfm, elapsed time",
                     (proc.time() - startTime)[3], "seconds.\n")
    return(dfmresult)
}

tokenizeSingle <- function(s, sep=" ", useclean=FALSE, ...) {
    if (useclean) s <- clean(s, ...)
    # s <- unlist(s)
    tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
    return(tokens)
}

#' @rdname dfm
#' @param groups Grouping variable for aggregating documents
#' @export
#' @examples
#' # sparse matrix from a corpus
#' mydfms <- dfm(inaugCorpus, matrixType="sparse")
#' data(ie2010Corpus, package="quantedaData")
#' mydfms2 <- dfm(ie2010Corpus, groups = "party", matrixType="sparse")
dfm.corpus <- function(x, verbose=TRUE, clean=TRUE, stem=FALSE, 
                        ignoredFeatures=NULL, 
                        keptFeatures=NULL,
                        matrixType="dense", language="english",
                        groups=NULL, bigrams=FALSE, 
                        thesaurus=NULL, dictionary=NULL, dictionary_regex=FALSE,
                        addto=NULL, ...) {
    if (verbose) cat("Creating a dfm from a corpus ...")
    
    if (!is.null(groups)) {
        if (verbose) cat("\n   ... grouping texts by variable", 
                         ifelse(length(groups)==1, "", "s"), ": ", 
                         paste(groups, collapse=", "), sep="")
        if (length(groups)>1) {
            group.split <- lapply(documents(x)[, groups], as.factor)
        } else {
            group.split <- as.factor(documents(x)[,groups])
        }
        texts <- split(texts(x), group.split)
        texts <- sapply(texts, paste, collapse = " ")
    } else {
        texts <- texts(x)
        names(texts) <- docnames(x)
    }
    
    dfm(texts, verbose=verbose, clean=clean, stem=stem, 
         ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
         matrixType=matrixType, language=language,
         thesaurus=thesaurus, dictionary=dictionary, dictionary_regex=dictionary_regex,
         fromCorpus=TRUE, bigrams=bigrams, addto=addto, ...)
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

#' @details \code{as.dfm} coerces a matrix or data.frame to a dfm
#' @rdname dfm
#' @export
as.dfm <- function(x) {
    if (!any((c("matrix", "data.frame") %in% class(x))))
        stop("as.dfm only applicable to matrix(-like) objects.")
    x <- as.matrix(x)
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
#' @param ... additional arguments passed to other methods
#' @export
topfeatures <- function(x, ...) {
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
topfeatures.dfm <- function(x, n=10, decreasing=TRUE, ci=.95, ...) {
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

#' @export
#' @rdname topfeatures
topfeatures.dgCMatrix <- function(x, n=10, decreasing=TRUE, ...) {
    if (is.null(n)) n <- ncol(x)
#     if (is.resampled(x)) {
#         subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
#         subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
#         return(data.frame(#features=colnames(subdfm),
#             freq=colSums(subdfm[,,1]),
#             cilo=apply(colSums(subdfm), 1, quantile, (1-ci)/2),
#             cihi=apply(colSums(subdfm), 1, quantile, 1-(1-ci)/2)))
#     } else {
        
    csums <- colSums(x)
    names(csums) <- x@Dimnames$features
    subdfm <- sort(csums, decreasing)
    return(subdfm[1:n])
#    }
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
#'   \item logTf - The natural log of the term frequency 
#'   \item tf-idf - Term-frequency * inverse 
#'   document frequency. For a full explanation, see, for example, 
#'   \url{(http://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html)}.
#'    This implementation will not return negative values. 
#'   \item maxTf - The term frequency divided 
#'   by the frequency of the most frequent term in the document 
#'   \item ppmi -   Positive Pointwise Mutual Information }
#' @param smooth amount to apply as additive smoothing to the document-feature matrix prior to
#'    weighting, default is 0.5, set to \code{smooth=0} for no smoothing.
#' @param ... not currently used
#' @return The dfm with weighted values
#' @export
#' @author Paul Nulty and Kenneth Benoit
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
weight <- function(x, ...) {
    UseMethod("weight")
}

#' @rdname weight
#' @export
weight.dfm <- function(x, type=c("normTf","maxTf","logTf", "tfidf", "ppmi"), smooth = 0.5, ...){
    attr_orig <- attributes(x)
    type <- match.arg(type)
    x <- x + smooth
    if (type=="normTf") {
        x <- x/rowSums(x)
    } else if (type=="maxTf") {
        x <- t(apply(dtm, 1, function(tef) tef/max(tef)))
    } else if (type=="logTf") {
        x <- 1 + log(x)
    } else if (type=="tfidf") {
        ## CHECK/REVISE
        idf <- log(ndoc(x)+1) - log(colSums(x > 0.5) + 1)
        x <- t(t(x) * idf)
    } else if (type=="ppmi") {
        pij <- x/rowSums(x)
        pij[is.nan(pij)] <- 0
        pi <- colSums(x)
        pj <- rowSums(x)
        pj[is.nan(pj)] <- 0
        pmi <- (pij / t(outer(pi,pj)))
        x <- abs(pmi)
    } else warning( sprintf("Type %s not implmented, no weighting performed.", type))
    x[is.infinite(x)] <- 0
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
# @export
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
