####################################################################
## dfm working and construction functions
##
## Ken Benoit
####################################################################


# @include dfm-classes.R
NULL

#' create a document-feature matrix
#' 
#' Create a sparse matrix document-feature matrix from a corpus or a vector of
#' texts.  The sparse matrix construction uses  the \pkg{Matrix} package, and is
#' both much faster and much more memory efficient than the corresponding dense
#' (regular \code{matrix}) representation.  For details on the structure of the
#' dfm class, see \link{dfm-class}.
#' 
#' @param x corpus or character vector from which to generate the
#'   document-feature matrix
#' @param ... additional arguments passed to \link{tokenize}, which can include for
#'   instance \code{ngrams} and \code{concatenator} for tokenizing multi-token
#'   sequences
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
#'   characters, see \link{tokenize} 
#' @param removeSeparators remove separators (whitespace), see \link{tokenize}
#' @param stem if \code{TRUE}, stem words
#' @param ignoredFeatures a character vector of user-supplied features to 
#'   ignore, such as "stop words".  To access one 
#'   possible list (from any list you wish), use \code{\link{stopwords}()}.  The
#'   pattern matching type will be set by \code{valuetype}.  For behaviour of 
#'   \code{ingoredFeatures} with \code{ngrams > 1}, see Details.
#' @param keptFeatures a use supplied regular expression defining which features
#'   to keep, while excluding all others.  This can be used in lieu of a 
#'   dictionary if there are only specific features that a user wishes to keep. 
#'   To extract only Twitter usernames, for example, set \code{keptFeatures = 
#'   "@@*"} and make sure that \code{removeTwitter = FALSE} as an additional 
#'   argument passed to \link{tokenize}.  Note: \code{keptFeatures = 
#'   "^@@\\\w+\\\b"} would be the regular expression version of this matching 
#'   pattern.  The pattern matching type will be set by \code{valuetype}.
#' @param dictionary A list of character vector dictionary entries, including 
#'   regular expressions (see examples)
#' @param thesaurus A list of character vector "thesaurus" entries, in a 
#'   dictionary list format, which can also include regular expressions  if 
#'   \code{dictionary_regex} is \code{TRUE} (see examples).  Note that unlike 
#'   dictionaries, each entry in a thesaurus key must be unique, otherwise only 
#'   the first match in the list will be used.  Thesaurus keys are converted to 
#'   upper case to create a feature label in the dfm, as a reminder that this 
#'   was not a type found in the text, but rather the label of a thesaurus key.
#' @param valuetype \code{fixed} for words as is; \code{"regex"} for regular 
#'   expressions; or \code{"glob"} for "glob"-style wildcard.  Glob format is 
#'   the default.  See \code{\link{selectFeatures}}.
#' @param dictionary_regex \code{TRUE} means the dictionary is already in 
#'   regular expression format, otherwise it will be converted from the "glob" 
#'   format.  This is a legacy argument that will soon be phased out in favour 
#'   of \code{valuetype}.
#' @param language Language for stemming.  Choices are 
#'   \code{danish}, \code{dutch}, \code{english}, \code{finnish}, \code{french},
#'   \code{german}, \code{hungarian}, \code{italian}, \code{norwegian}, 
#'   \code{porter}, \code{portuguese}, \code{romanian}, \code{russian}, 
#'   \code{spanish}, \code{swedish}, \code{turkish}.
#' @param matrixType deprecated, used to produce a dense matrix if \code{dense},
#'   but this was removed in 0.8.2.  All dfm objects are now created as a sparse
#'   matrix of class \code{dgCMatrix} from the \pkg{\link{Matrix}} package.
#' @return A \link{dfm-class} object containing a sparse matrix representation 
#'   of the counts of features by document, along with associated settings and 
#'   metadata.
#' @details The default behavior for \code{ignoredFeatures} when constructing
#'   ngrams using \code{dfm(x, } \emph{ngrams > 1}\code{)} is to remove any ngram that
#'   contains any item in \code{ignoredFeatures}.  If you wish to remove these before
#'   constructing ngrams, you will need to first tokenize the texts with ngrams, then
#'   remove the features to be ignored, and then construct the dfm using this modified
#'   tokenization object.  See the code examples for an illustration.
#' @author Kenneth Benoit
#' @importFrom parallel mclapply
#' @import data.table Matrix
#' @export
#' @examples
#' # why we phased out dense matrix dfm objects
#' (size1 <- object.size(dfm(inaugTexts, verbose = FALSE)))
#' (size2 <- object.size(as.matrix(dfm(inaugTexts, verbose = FALSE))))
#' cat("Compacted by ", round(as.numeric((1-size1/size2)*100), 1), "%.\n", sep="")
#' 
#' # for a corpus
#' mydfm <- dfm(subset(inaugCorpus, Year>1980))
#' mydfm <- dfm(subset(inaugCorpus, Year>1980), toLower=FALSE)
#' 
#' # grouping documents by docvars in a corpus
#' mydfmGrouped <- dfm(subset(inaugCorpus, Year>1980), groups = "President")
#' 
#' # with English stopwords and stemming
#' dfmsInaug2 <- dfm(subset(inaugCorpus, Year>1980), 
#'                   ignoredFeatures=stopwords("english"), stem=TRUE)
#' # works for both words in ngrams too
#' dfm("Banking industry", stem = TRUE, ngrams = 2, verbose = FALSE)
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
#' dfm(phrasetotoken(mytexts, mydict), thesaurus = lapply(mydict, function(x) gsub("\\s", "_", x)))
#' # pick up "taxes" with "tax" as a regex
#' dfm(phrasetotoken(mytexts, mydict), thesaurus = list(anytax = "tax"), valuetype = "regex")
#' 
#' # removing stopwords
#' testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
#'              the newspaper from a boy named Seamus, in his mouth."
#' testCorpus <- corpus(testText)
#' # note: "also" is not in the default stopwords("english")
#' features(dfm(testCorpus, ignoredFeatures = stopwords("english")))
#' # for ngrams
#' features(dfm(testCorpus, ngrams = 2, ignoredFeatures = stopwords("english")))
#' features(dfm(testCorpus, ngrams = 1:2, ignoredFeatures = stopwords("english")))
#' 
#' ## removing stopwords before constructing ngrams
#' tokensAll <- tokenize(toLower(testText), removePunct = TRUE)
#' tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
#' tokensNgramsNoStopwords <- ngrams(tokensNoStopwords, 2)
#' features(dfm(tokensNgramsNoStopwords, ngrams = 1:2))
#' 
#' # keep only certain words
#' dfm(testCorpus, keptFeatures = "*s", verbose = FALSE)  # keep only words ending in "s"
#' dfm(testCorpus, keptFeatures = "s$", valuetype = "regex", verbose = FALSE)
#' 
#' # testing Twitter functions
#' testTweets <- c("My homie @@justinbieber #justinbieber shopping in #LA yesterday #beliebers",
#'                 "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(testTweets, keptFeatures = "#*", removeTwitter = FALSE)  # keep only hashtags
#' dfm(testTweets, keptFeatures = "^#.*$", valuetype = "regex", removeTwitter = FALSE)
#' 
dfm.character <- function(x, 
                          verbose=TRUE, 
                          toLower=TRUE, 
                          removeNumbers = TRUE, 
                          removePunct = TRUE,
                          removeSeparators = TRUE,
                          removeTwitter = FALSE,
                          # removeCurrency = TRUE,
                          # removeURL = TRUE,
                          stem = FALSE, 
                          ignoredFeatures = NULL, 
                          keptFeatures=NULL,
                          matrixType=c("sparse", "dense"), 
                          language="english",
                          thesaurus=NULL, 
                          dictionary=NULL,
                          valuetype = c("glob", "regex", "fixed"),
                          dictionary_regex = FALSE, 
                          ...) {
    startTime <- proc.time()
    matrixType <- match.arg(matrixType)
    valuetype <- match.arg(valuetype)
    
    if (verbose && grepl("^dfm\\.character", sys.calls()[[2]]))
        cat("Creating a dfm from a character vector ...")

    # case conversion and tokenization
    # includes bigram tokenization but this will be merged soon into tokenize()
    if (toLower) {
        if (verbose) cat("\n   ... lowercasing", sep="")
        x <- toLower(x)
    }
    
    if (verbose) cat("\n   ... tokenizing", sep="")
    tokenizedTexts <- tokenize(x, removeNumbers = removeNumbers, 
                               removeSeparators = removeSeparators, removePunct = removePunct,
                               removeTwitter = removeTwitter,
                               ...)

    # if ngrams > 1 and ignoredFeatures are specified, then convert these into a
    # regex that will remove any ngram containing one of the words
#     if (!identical(attr(tokenizedTexts, "ngrams"), 1) & !is.null(ignoredFeatures)) {
#         conc <- attr(tokenizedTexts, "concatenator")
#         if (valuetype == "glob") {
#             ignoredFeatures <- gsub("\\*", ".*", ignoredFeatures)
#             ignoredFeatures <- gsub("\\?", ".{1}", ignoredFeatures)
#         }
#         ignoredFeatures <- paste0("(\\b|(\\w+", conc, ")+)", ignoredFeatures, "(\\b|(", conc, "\\w+)+)")
#         valuetype <- "regex"
#     }
    
    dfm(tokenizedTexts, verbose=verbose, toLower=toLower, stem=stem, 
        ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
        matrixType=matrixType, language=language,
        thesaurus=thesaurus, dictionary=dictionary, valuetype = valuetype, 
        dictionary_regex = dictionary_regex,
        startTime = startTime)
}

    
#' @rdname dfm
#' @importFrom utils glob2rx
#' @export
dfm.tokenizedTexts <- function(x, 
                               verbose=TRUE,
                               toLower = TRUE,
                               stem=FALSE, 
                               ignoredFeatures=NULL, 
                               keptFeatures=NULL,
                               matrixType=c("sparse", "dense"), 
                               language="english",
                               thesaurus=NULL, 
                               dictionary=NULL, 
                               valuetype = c("glob", "regex", "fixed"),
                               dictionary_regex = FALSE,
                               ...) {
    
    settings_ngrams <- attr(x, "ngrams")
    settings_concatenator <- attr(x, "concatenator")
    
    valuetype <- match.arg(valuetype)
    dots <- list(...)
    if (length(dots) && any(!(names(dots)) %in% c("startTime", "codeType")))
        warning("Argument", ifelse(length(dots)>1, "s ", " "), names(dots), " not used.", sep = "", noBreaks. = TRUE)
    
    startTime <- proc.time()
    if ("startTime" %in% names(dots)) startTime <- dots$startTime
    
    if ("codeType" %in% names(dots))
        return(dfmTokenizeTextsOld(x, verbose=verbose, toLower=toLower, stem=stem, 
                                   ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
                                   matrixType=matrixType, language=language,
                                   thesaurus=thesaurus, dictionary=dictionary, dictionary_regex=dictionary_regex,
                                   startTime = startTime))
    
    # argument checking
    matrixType <- match.arg(matrixType)
    if (matrixType == "dense")
        warning("matrixType = \"dense\" no longer supported, created sparse dfm instead")
    language <- tolower(language)
    
    if (verbose && grepl("^dfm\\.tokenizedTexts", sys.calls()[[2]])) {
        cat("Creating a dfm from a tokenizedTexts object ...")
    }

    # add a "NULL" token to every tokenized text, in case some are empty
    x <- lapply(x, function(t) t <- c(t, "**_NULL_**"))
    class(x) <- c("tokenizedTexts", "list")
    
    # get document names
    if (is.null(names(x))) {
        docNames <- paste("text", 1:length(x), sep="")
    } else docNames <- names(x)
    
    # index documents
    if (verbose) cat("\n   ... indexing documents: ", 
                     format(length(x), big.mark=","), " document",
                     ifelse(length(x) > 1, "s", ""), sep="")
    nTokens <- lengths(x)
    docIndex <- rep(seq_along(nTokens), nTokens)
    
    # index features
    if (verbose) cat("\n   ... indexing features: ")
    allFeatures <- unlist(x)
    uniqueFeatures <- unique(allFeatures)
    totalfeatures <- length(uniqueFeatures)
    if (verbose) cat(format(totalfeatures - 1, big.mark=","), " feature type",
                     ifelse(totalfeatures - 1  > 1, "s", ""), sep="")
    featureIndex <- match(allFeatures, uniqueFeatures)
    
    if (verbose) cat("\n")
    
    # make the dfm
    dfmresult <- sparseMatrix(i = docIndex, 
                              j = featureIndex, 
                              x = 1L, 
                              dimnames = list(docs = docNames, features = uniqueFeatures))
    # remove null term
    dfmresult <- dfmresult[, -match("**_NULL_**", colnames(dfmresult)), drop = FALSE]
    # construct the dfmSparse type object
    dfmresult <- new("dfmSparse", dfmresult)
    
    # copy attributes
    dfmresult@ngrams <- settings_ngrams
    dfmresult@concatenator <- settings_concatenator
    
    if (dictionary_regex & valuetype != "regex") {
        warning("dictionary_regex is deprecated, use valuetype = \"regex\" instead.")
        valuetype <- "regex"
    }
    
    if (!is.null(dictionary) | !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- thesaurus
        if (verbose) cat("   ... ")
        dfmresult <- applyDictionary(dfmresult, dictionary,
                                     exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                                     valuetype = valuetype,
                                     verbose = verbose)
    }
    
    if (!is.null(ignoredFeatures)) {
        if (verbose) cat("   ... ")
        dfmresult <- selectFeatures(dfmresult, ignoredFeatures, selection = "remove", valuetype = valuetype, verbose = verbose)
    }
    
    if (!is.null(keptFeatures)) {
        if (verbose) cat("   ... ")
        dfmresult <- selectFeatures(dfmresult, keptFeatures, selection = "keep", valuetype = valuetype, verbose = verbose)
    }
    
    if (stem) {
        if (verbose) cat("   ... stemming features (", stri_trans_totitle(language), ")", sep="")
        oldNfeature <- nfeature(dfmresult)
        dfmresult <- wordstem(dfmresult, language)
        if (verbose) 
            if (oldNfeature - nfeature(dfmresult) > 0) 
                cat(", trimmed ", oldNfeature - nfeature(dfmresult), " feature variant",
                    ifelse(oldNfeature - nfeature(dfmresult) != 1, "s", ""), "\n", sep = "")
        else
            cat("\n")
    }
    
    if (verbose) 
        cat("   ... created a", paste(dim(dfmresult), collapse=" x "), 
            "sparse dfm\n   ... complete. \nElapsed time:", (proc.time() - startTime)[3], "seconds.\n")
    
    return(dfmresult)
}

    
    
dfmTokenizeTextsOld <- function(x, 
                               verbose=TRUE,
                               toLower = TRUE,
                               stem=FALSE, 
                               ignoredFeatures=NULL, 
                               keptFeatures=NULL,
                               matrixType=c("sparse", "dense"), 
                               language="english",
                               thesaurus=NULL, 
                               dictionary=NULL, 
                               dictionary_regex=FALSE,
                               ...) {
    dots <- list(...)
    if ("startTime" %in% names(dots)) startTime <- dots$startTime
    
    matrixType <- match.arg(matrixType)

    if (verbose && grepl("^dfm\\.tokenizedTexts", sys.calls()[[2]])) {
        cat("Creating a dfm from a tokenizedTexts object ...")
        startTime <- proc.time()
    }
    
    # index documents
    if (verbose) cat("\n   ... indexing ", 
                     format(length(x), big.mark=","), " document",
                     ifelse(length(x) > 1, "s", ""), sep="")
    docIndex <- 1:length(x)
    if (is.null(names(x))) 
        names(docIndex) <- factor(paste("text", 1:length(x), sep="")) else
            names(docIndex) <- names(x)

    # index features
    if (verbose) cat("\n   ... shaping tokens into data.table")
    alltokens <- data.table(docIndex = rep(docIndex, sapply(x, length)),
                            features = unlist(x, use.names = FALSE))
    alltokens <- alltokens[features != ""]  # if there are any "blank" features
    if (verbose) cat(", found", format(nrow(alltokens), big.mark=","), "total tokens")
#     if (verbose & bigrams) 
#         cat(" incl.", format(sum(grepl("_", alltokens$features)), big.mark=","), "bigrams")
    
    # stemming features
    if (stem == TRUE) {
        language <- tolower(language)
        if (!(language %in% SnowballC::getStemLanguages())) {
            cat("\n   ... WARNING: not stemming because language", language, "is unavailable")
        } else {
            if (verbose) cat("\n   ... stemming the tokens (", language, ")", sep="")
            # parallelization with just two cores seems to speed things up by x2
            # alltokens[, features := simplify2array(mclapply(alltokens$features, wordstem, language=language))]
            alltokens[, features := wordstem(alltokens$features, language=language)]
        }
    }
    
    # "stop words" through ignoredFeatures
    if (!is.null(ignoredFeatures)) {
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
            thesaurus <- lapply(thesaurus, utils::glob2rx) # makeRegEx)
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
        # flatten the dictionary
        dictionary <- flatten.dictionary(dictionary)
        if (verbose) cat("consisting of ", length(dictionary), " key entr", 
                         ifelse(length(dictionary) > 1, "ies", "y"), sep="")
        # convert wildcards to regular expressions (if needed)
        if (!dictionary_regex)
            dictionary <- lapply(dictionary, utils::glob2rx) # makeRegEx)
        # lowercase the dictionary if toLower == TRUE
        if (toLower) dictionary <- lapply(dictionary, toLower)
        # call the dictionary entry counting function and return new alltokens
        alltokens <- countDictionaryEntries(alltokens, dictionary)
    }
    
    n <- NULL
    if (verbose) cat("\n   ... summing", ifelse(is.null(dictionary), "tokens", "dictionary-matched features"), "by document")
    alltokens[, "n":=1L]
    alltokens <- alltokens[, by=list(docIndex,features), sum(n)]
    
    if (verbose) cat("\n   ... indexing ")
    uniqueFeatures <- unique(alltokens$features)
    
    # now remove the docIndex == 0, now that all dictionary keys are indexed as features
    alltokens <- alltokens[docIndex > 0]
    
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
    suppressWarnings(alltokens <- alltokens[featureTable, allow.cartesian = TRUE])
    alltokens[is.na(docIndex), c("docIndex", "V1") := list(1, 0)]
    if (verbose) cat("\n   ... building sparse matrix")
    #suppressWarnings(
    dfmresult <- sparseMatrix(i = alltokens$docIndex, 
                              j = alltokens$featureIndex, 
                              x = alltokens$V1, 
                              dimnames=list(docs=names(docIndex), features=uniqueFeatures))
    #    )
    
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
    
    # make into sparse S4 class inheriting from dgCMatrix
    dfmresult <- new("dfmSparse", dfmresult)
    
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
        cat("\n   ... complete. \nElapsed time:", (proc.time() - startTime)[3], "seconds.\n")
    }
    if (matrixType == "dense")
        cat("  Note: matrixType dense is being phased out, try sparse instead.\n")
    
    return(dfmresult)
}

#' @rdname dfm
#' @param groups character vector containing the names of document variables for
#'   aggregating documents
#' @export
dfm.corpus <- function(x, verbose = TRUE, groups = NULL, ...) {
    if (verbose) cat("Creating a dfm from a corpus ...")
    
    if (!is.null(groups)) {
        groupsLab <- ifelse(is.factor(groups), deparse(substitute(groups)), groups)
        if (verbose) cat("\n   ... grouping texts by variable", 
                         ifelse(length(groupsLab)==1, "", "s"), ": ", 
                         paste(groupsLab, collapse=", "), sep="")
        texts <- texts(x, groups = groups)
    } else {
        texts <- texts(x)
        names(texts) <- docnames(x)
    }
    
    dfm(texts, verbose = verbose, ...)
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

countDictionaryEntries <- function(alltokens, dictionary) {
    dictIndex <- docIndex <- word <- NULL
    # get unique tokens from all tokens
    alltokensFeatures <- unique(alltokens$features)
    # create a data table of all dictionary keys and entries as single regexes
    allDictEntries <- data.table(dictIndex = names(dictionary), 
                                 regex = sapply(dictionary, paste, collapse = "|", USE.NAMES = FALSE))
    # match unique tokens in dictionary to each dictionary category
    alltokensFeaturesInDict <- alltokensFeatures[stri_detect_regex(alltokensFeatures, 
                                                                   paste(allDictEntries$regex, collapse = "|"))]
    # now lookup each unique word in each dictionary category
    tmp <- parallel::mclapply(alltokensFeaturesInDict, function(x) which(stri_detect_regex(x, allDictEntries$regex)))
    # make list of matches into a data.table
    tmp <- data.table(word = rep(alltokensFeaturesInDict, sapply(tmp, length)),
                      features = allDictEntries[unlist(tmp), dictIndex])
    # use data.table merging to merge with original set of features
    setkey(tmp, word)
    setkey(alltokens, features)
    merged <- tmp[alltokens, allow.cartesian=TRUE]
    merged <- merged[!is.na(features), list(docIndex, features)]
    # paste the empty categories too as docIndex 0 and return
    rbind(data.table(docIndex = 0, features = names(dictionary)), merged)
}


