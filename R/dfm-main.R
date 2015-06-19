####################################################################
## dfm working and construction functions
##
## Ken Benoit
####################################################################


# @include dfm-classes.R
NULL

#' create a document-feature matrix
#' 
#' Create a sparse matrix document-feature matrix from a corpus or a vector of texts.  The sparse
#' matrix construction uses  the
#' \pkg{Matrix} package, and is both much faster and much more memory efficient
#' than the corresponding dense (regular \code{matrix}) representation.  For details on the
#' structure of the dfm class, see \link{dfm-class}.
#' 
#' New as of v0.7: All dfms are by default sparse, a change from the previous behaviour.  
#' You can still create the older (S3) dense matrix type dfm object, but you will receive
#' a disapproving warning message while doing so, suggesting you make the switch.
#' @param x corpus or character vector from which to generate the document-feature matrix
#' @param ... additional arguments passed to \code{\link{clean}}
#' @import Matrix
#' @export
#' @name dfm
dfm <- function(x, ...) {
    UseMethod("dfm")
}

#' @rdname dfm
#' @param verbose display messages if \code{TRUE}
#' @param toLower convert texts to lowercase
#' @param removeNumbers remove numbers, see \link{tokenize}
#' @param removePunct remove numbers, see \link{tokenize}
#' @param removeTwitter if \code{FALSE}, preserve \code{#} and \code{@@}
#'   characters, see \link{tokenize} #' @param removeSeparators remove
#'   separators (whitespace), see \link{tokenize}
#' @param stem if \code{TRUE}, stem words
#' @param ignoredFeatures a character vector of user-supplied features to 
#'   ignore, such as "stop words".  Formerly, this was a Boolean option for 
#'   \code{stopwords = TRUE}, but requiring the user to supply the list 
#'   highlights the choice involved in using any stopword list.  To access one 
#'   possible list (from any list you wish), use \code{\link{stopwords}()}.
#' @param keptFeatures a use supplied regular expression defining which features
#'   to keep, while excluding all others.  This can be used in lieu of a 
#'   dictionary if there are only specific features that a user wishes to keep. 
#'   To extract only Twitter usernames, for example, set \code{keptFeatures = 
#'   "^@@\\\w+\\\b"} and make sure that \code{removeTwitter = FALSE} as an 
#'   additional argument passed to \link{clean}.  (Note: \code{keptFeatures = 
#'   "^@@"} will also retrieve usernames, but does not enforce the username 
#'   convention that a username must contain one and only one \code{@@} symbol, 
#'   at the beginning of the username.)
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
#' @param include.unigrams exclude unigrams if \code{TRUE}; only used if 
#'   \code{bigrams=TRUE}
#' @param addto \code{NULL} by default, but if an existing dfm object is 
#'   specified, then the new dfm will be added to the one named. If both 
#'   \link{dfm}'s are built from dictionaries, the combined dfm will have its 
#'   \code{Non_Dictionary} total adjusted.
#' @param language Language for stemming and stopwords.  Choices are 
#'   \code{danish}, \code{dutch}, \code{english}, \code{finnish}, \code{french},
#'   \code{german}, \code{hungarian}, \code{italian}, \code{norwegian}, 
#'   \code{porter}, \code{portuguese}, \code{romanian}, \code{russian}, 
#'   \code{spanish}, \code{swedish}, \code{turkish} for stemming, and 
#'   \code{SMART}, \code{danish}, \code{english}, \code{french}, 
#'   \code{hungarian}, \code{norwegian}, \code{russian}, \code{swedish}, 
#'   \code{catalan}, \code{dutch}, \code{finnish}, \code{german}, 
#'   \code{italian}, \code{portuguese}, \code{spanish}, \code{arabic} for 
#'   stopwords.
#' @param matrixType if \code{dense}, produce a dense matrix; or it 
#'   \code{sparse} produce a sparse matrix of class \code{dgCMatrix} from the 
#'   \pkg{\link{Matrix}} package.
#' @return A \link{dfm-class} object containing a sparse matrix representation 
#'   of the counts of features by document, along with associated settings and 
#'   metadata.
#'   
#'   If you used \code{matrixType = "dense"} then the return is an old-style S3 
#'   matrix class object with additional attributes representing meta-data.
#' @author Kenneth Benoit
#' @importFrom parallel mclapply
#' @import data.table Matrix
#' @export
#' @examples
#' \donttest{# with inaugural texts
#' (size1 <- object.size(dfm(inaugTexts, matrixType="sparse")))
#' (size2 <- object.size(dfm(inaugTexts, matrixType="dense")))
#' cat("Compacted by ", round(as.numeric((1-size1/size2)*100), 1), "%.\n", sep="")
#' }
#' 
#' # for a corpus
#' mydfm <- dfm(subset(inaugCorpus, Year>1980))
#' mydfm <- dfm(subset(inaugCorpus, Year>1980), toLower=FALSE)
#' 
#' # grouping documents by docvars in a corpus
#' mydfmGrouped <- dfm(subset(inaugCorpus, Year>1980), groups = "President")
#' 
#' # with stopwords English, stemming, and dense matrix
#' dfmsInaug2 <- dfm(subset(inaugCorpus, Year>1980), 
#'                   ignoredFeatures=stopwords("english"),
#'                   stem=TRUE, matrixType="dense")
#' 
#' # with dictionaries
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
#' # with the thesaurus feature
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised a taxes: an income tax and a sales tax.")
#' mydict <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax")))
#' dfm(phrasetotoken(mytexts, mydict), thesaurus=lapply(mydict, function(x) gsub("\\s", "_", x)))
#' # pick up "taxes" with "tax" as a regex
#' dfm(phrasetotoken(mytexts, mydict), thesaurus=list(anytax="tax"), dictionary_regex=TRUE)
#' 
#' # removing stopwords
#' testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
#'              the newspaper from a boy named Seamus, in his mouth."
#' testCorpus <- corpus(testText)
#' # settings(testCorpus, "stopwords")
#' dfm(testCorpus, ignoredFeatures=stopwords("english"))
#' features(dfm(testCorpus, verbose=FALSE, bigrams=TRUE))
#' features(dfm(testCorpus, verbose=FALSE, bigrams=TRUE, include.unigrams=FALSE))
#' 
#' # keep only certain words
#' dfm(testCorpus, keptFeatures="s$", verbose=FALSE)  # keep only words ending in "s"
#' 
#' # testing Twitter functions
#' testTweets <- c("My homie @@justinbieber #justinbieber shopping in #LA yesterday #beliebers",
#'                 "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(testTweets, keptFeatures="^#", removePunct=FALSE)  # keep only hashtags
#' ## NOT WHAT WE WERE EXPECTING - NEED TO FIX
#' 
#' \dontrun{
#' # try it with approx 35,000 court documents from Lauderdale and Clark (200?)
#' load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
#' txts <- unlist(Opinion_files[1])
#' names(txts) <- NULL
#' system.time(dfmsBig <- dfm(txts))
#' object.size(dfmsBig)
#' 
#' # compare with tm
#' require(tm)
#' tmcorp <- VCorpus(VectorSource(txts))
#' system.time(tmDTM <- DocumentTermMatrix(tmcorp))
#' object.size(tmDTM)
#' }
dfm.character <- function(x, verbose=TRUE, 
                          toLower=TRUE, 
                          removeNumbers = TRUE, 
                          removePunct = TRUE,
                          removeSeparators = TRUE,
                          removeTwitter = TRUE,
                          # removeCurrency = TRUE,
                          # removeURL = TRUE,
                          stem=FALSE, 
                          ignoredFeatures = NULL, 
                          keptFeatures=NULL,
                          matrixType=c("sparse", "dense"), 
                          language="english",
                          bigrams=FALSE,
                          include.unigrams=TRUE,
                          thesaurus=NULL, 
                          dictionary=NULL, 
                          dictionary_regex=FALSE, 
                          addto=NULL, 
                          ...) {
    startTime <- proc.time()
    matrixType <- match.arg(matrixType)
    
    if (verbose && grepl("^dfm\\.character", sys.calls()[[2]]))
        cat("Creating a dfm from a character vector ...")
    
    # index documents
    if (verbose) cat("\n   ... indexing ", 
                     format(length(x), big.mark=","), " document",
                     ifelse(length(x) > 1, "s", ""), sep="")
    docIndex <- 1:length(x)
    if (is.null(names(x))) 
        names(docIndex) <- factor(paste("text", 1:length(x), sep="")) else
            names(docIndex) <- names(x)
    
    # case conversion and tokenization
    # includes bigram tokenization but this will be merged soon into tokenize()
    if (toLower) {
        if (verbose) cat("\n   ... lowercasing", sep="")
        x <- toLower(x)
    }
    
    if (!bigrams) {
        if (verbose) cat("\n   ... tokenizing", sep="")
        tokenizedTexts <- tokenize(x, toLower=toLower, removeNumbers=removeNumbers, 
                                   removeSeparators=removeSeparators, removePunct=removePunct,
                                   removeTwitter = removeTwitter)
    } else {
        if (verbose) cat("\n   ... forming bigrams", sep="")
        tokenizedTexts <- bigrams(x, ignoredFeatures = ignoredFeatures, include.unigrams=include.unigrams)
    }
    
    # index features
    if (verbose) cat("\n   ... shaping tokens into data.table")
    alltokens <- data.table(docIndex = rep(docIndex, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names = FALSE))
    alltokens <- alltokens[features != ""]  # if there are any "blank" features
    if (verbose) cat(", found", format(nrow(alltokens), big.mark=","), "total tokens")
    if (verbose & bigrams) 
        cat(" incl.", format(sum(grepl("_", alltokens$features)), big.mark=","), "bigrams")
    
    # stemming features
    if (stem == TRUE) {
        language <- tolower(language)
        if (!(language %in% SnowballC::getStemLanguages())) {
            cat("\n   ... WARNING: not stemming because language", language, "is unavailable")
        } else {
            if (verbose) cat("\n   ... stemming the tokens (", language, ")", sep="")
            # parallelization with just two cores seems to speed things up by x2
            alltokens[, features := simplify2array(mclapply(alltokens$features, wordstem, language=language))]
        }
    }
    
    # "stop words" through ignoredFeatures
    if (!is.null(ignoredFeatures) & !bigrams) {
        if (!is.character(ignoredFeatures)) {
            cat("\n   ... WARNING: not ignoring words because not a character vector")
        } else {
            if (verbose) cat("\n   ... ignoring", format(length(ignoredFeatures), big.mark=","), "feature types, discarding ")
            # this is slower but removes all bigrams containing stop words
            # ignoredfeatIndex <- grep(paste0("\\b", paste(ignoredFeatures, collapse="\\b|\\b"), "\\b"), gsub("_", " ", alltokens$features))
            ignoredfeatIndex <- which(alltokens$features %in% ignoredFeatures)
            if (verbose) {
                cat(format(length(ignoredfeatIndex), big.mark=","), " total features (",
                    format(length(ignoredfeatIndex) / nrow(alltokens) * 100, digits=3),
                    "%)", sep="")
            }
            if (length(ignoredfeatIndex) > 0) alltokens <- alltokens[!ignoredfeatIndex]
        }
    }
    
    # thesaurus to make word equivalencies
    if (!is.null(thesaurus)) {
        thesaurus <- flatten.dictionary(thesaurus)
        if (!dictionary_regex)
            thesaurus <- lapply(thesaurus, glob2rx) # makeRegEx)
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
            dictionary <- lapply(dictionary, glob2rx) # makeRegEx)
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
        n <- NULL
        if (verbose) cat("\n   ... summing tokens by document")
        alltokens[, "n":=1L]
        alltokens <- alltokens[, by=list(docIndex,features), sum(n)]
        
        if (verbose) cat("\n   ... indexing ")
        uniqueFeatures <- unique(alltokens$features)
        ## BETTER METHOD, BUT SLOWER, IS stri_unique()
        uniqueFeatures <- sort(uniqueFeatures)
        # are any features the null string?
        blankFeatureIndex <- which(uniqueFeatures == "")
        totalfeatures <- length(uniqueFeatures) - (length(blankFeatureIndex) > 0)
        if (verbose) cat(format(totalfeatures, big.mark=","), " feature type",
                         ifelse(totalfeatures > 1, "s", ""), sep="")
        # much faster than using factor(alltokens$features, levels=uniqueFeatures) !!
        featureTable <- data.table(featureIndex = 1:length(uniqueFeatures),
                                   features = uniqueFeatures)
        setkey(alltokens, features)
        setkey(featureTable, features)
        # merge, data.table style.  warnings suppressed or it moans about mixed encodings
        ## suppressWarnings(alltokens <- alltokens[featureTable])
        alltokens <- alltokens[featureTable]
        
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
        if (length(blankFeatureIndex) > 0) dfmresult <- dfmresult[, -blankFeatureIndex]
    }
    
    # make into sparse S4 class inheriting from dgCMatrix
    dfmresult <- new("dfmSparse", dfmresult)
    
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
        
        #dfmS4 <- setClass("dfm", contains = "dgCMatrix")
        #dfmresult <- dfmS4(dfmresult)
    }
    
    # add settings as an attribute
    # attr(resultdfm, "settings") <- settings(x)
    # class and label dimnames if an array
    if (matrixType == "dense") {
        if (verbose) cat("\n   ... converting to a dense matrix")
        dfmresult <- as.matrix(dfmresult)
        class(dfmresult) <- c("dfm", class(dfmresult))
        attr(dfmresult, "weighting") <- "frequency"
    }
    
    if (verbose) {
        cat("\n   ... created a", paste(dim(dfmresult), collapse=" x "), 
            ifelse(matrixType=="dense", "dense", "sparse"), "dfm")
        cat("\n   ... complete. Elapsed time:", (proc.time() - startTime)[3], "seconds.\n")
    }
    if (matrixType == "dense")
        cat("  Note: matrixType dense is being phased out, try sparse instead.\n")
    return(dfmresult)
}

#' @rdname dfm
#' @export
dfm.tokenizedTexts <- function(x, verbose=TRUE, toLower=TRUE, stem=FALSE, 
                               ignoredFeatures=NULL, 
                               keptFeatures=NULL,
                               matrixType=c("sparse", "dense"), language="english",
                               groups=NULL, bigrams=FALSE, 
                               include.unigrams=TRUE,
                               thesaurus=NULL, dictionary=NULL, dictionary_regex=FALSE,
                               addto=NULL, ...) {
    
    ## should check that options in ... do NOT include tokenization options
    
    .TOKENIZE <- FALSE
    
    dfm(texts, verbose=verbose, toLower=toLower, stem=stem, 
        ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
        matrixType=matrixType, language=language,
        thesaurus=thesaurus, dictionary=dictionary, dictionary_regex=dictionary_regex,
        fromCorpus=TRUE, bigrams=bigrams, include.unigrams=include.unigrams,
        addto=addto, ...)
}

#' @rdname dfm
#' @param groups character vector containing the names of document variables for
#'   aggregating documents
#' @export
dfm.corpus <- function(x, verbose=TRUE, toLower=TRUE, stem=FALSE, 
                       ignoredFeatures=NULL, 
                       keptFeatures=NULL,
                       matrixType=c("sparse", "dense"), language="english",
                       groups=NULL, bigrams=FALSE, 
                       include.unigrams=TRUE,
                       thesaurus=NULL, dictionary=NULL, dictionary_regex=FALSE,
                       addto=NULL, ...) {
    if (verbose) cat("Creating a dfm from a corpus ...")
    
    if (!is.null(groups)) {
        if (verbose) cat("\n   ... grouping texts by variable", 
                         ifelse(length(groups)==1, "", "s"), ": ", 
                         paste(groups, collapse=", "), sep="")
        texts <- texts(x, groups = groups)
    } else {
        texts <- texts(x)
        names(texts) <- docnames(x)
    }
    
    dfm(texts, verbose=verbose, toLower=toLower, stem=stem, 
        ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
        matrixType=matrixType, language=language,
        thesaurus=thesaurus, dictionary=dictionary, dictionary_regex=dictionary_regex,
        bigrams=bigrams, include.unigrams=include.unigrams,
        addto=addto, ...)
}



# Flatten a hierarchical dictionary into a list of character vectors
#
# Converts a hierarchical dictionary (a named list of named lists, ending in character
# vectors at the lowest level) into a flat list of character vectors.  Works like
# \code{unlist(dictionary, recursive=TRUE)} except that the recursion does not go to the
# bottom level.
#
# Called by dfm()
#
# @param elms list to be flattened
# @param parent parent list name, gets built up through recursion in the same way that \code{unlist(dictionary, recursive=TRUE)} works
# @param dict the bottom list of dictionary entries ("synonyms") passed up from recursive calls
# @return A dictionary flattened down one level further than the one passed
# @export
# @author Kohei Watanabe
# @examples
# dictPopulismEN <-
#     list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
#                     "corrupt*", "propagand", "politici*", "*deceit*",
#                     "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
#                     "dishonest*", "establishm*", "ruling*"))
# flatten.dictionary(dictPopulismEN)
#
# hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#                              level1a2 = c("l1a21", "l1a22")),
#               level1b = list(level1b1 = c("l1b11", "l1b12"),
#                              level1b2 = c("l1b21", "l1b22", "l1b23")),
#               level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#                              level1c1b = list(level1c1b1 = c("lowestalone"))))
# flatten.dictionary(hdict)
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


