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
#' @param removePunct remove punctuation, see \link{tokenize}
#' @param removeTwitter if \code{FALSE}, preserve \code{#} and \code{@@} 
#'   characters, see \link{tokenize}
#' @param removeSeparators remove separators (whitespace), see \link{tokenize}
#' @param stem if \code{TRUE}, stem words
#' @param ignoredFeatures a character vector of user-supplied features to 
#'   ignore, such as "stop words".  To access one possible list (from any list
#'   you wish), use \code{\link{stopwords}()}.  The pattern matching type will
#'   be set by \code{valuetype}.  For behaviour of \code{ingoredFeatures} with
#'   \code{ngrams > 1}, see Details.
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
#'   dictionary list format, which operates as a dictionary but without
#'   excluding values not matched from the dictionary.  Thesaurus keys are
#'   converted to upper case to create a feature label in the dfm, as a reminder
#'   that this was not a type found in the text, but rather the label of a
#'   thesaurus key.  For more fine-grained control over this and other aspects
#'   of converting features into dictionary/thesaurus keys from pattern matches
#'   to values, you can use \code{\link{applyDictionary}} after creating the
#'   dfm.
#' @param valuetype \code{fixed} for words as is; \code{"regex"} for regular 
#'   expressions; or \code{"glob"} for "glob"-style wildcard.  Glob format is 
#'   the default.  See \code{\link{selectFeatures}}.
#' @param language Language for stemming.  Choices are \code{danish},
#'   \code{dutch}, \code{english}, \code{finnish}, \code{french}, \code{german},
#'   \code{hungarian}, \code{italian}, \code{norwegian}, \code{porter},
#'   \code{portuguese}, \code{romanian}, \code{russian}, \code{spanish},
#'   \code{swedish}, \code{turkish}.
#' @return A \link{dfm-class} object containing a sparse matrix representation 
#'   of the counts of features by document, along with associated settings and 
#'   metadata.
#' @details The default behavior for \code{ignoredFeatures} when constructing 
#'   ngrams using \code{dfm(x, } \emph{ngrams > 1}\code{)} is to remove any
#'   ngram that contains any item in \code{ignoredFeatures}.  If you wish to
#'   remove these before constructing ngrams, you will need to first tokenize
#'   the texts with ngrams, then remove the features to be ignored, and then
#'   construct the dfm using this modified tokenization object.  See the code
#'   examples for an illustration.
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
#' features(dfm(tokensNgramsNoStopwords, verbose = FALSE))
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
                          language="english",
                          thesaurus=NULL, 
                          dictionary=NULL,
                          valuetype = c("glob", "regex", "fixed"),
                          ...) {
    startTime <- proc.time()
    valuetype <- match.arg(valuetype)
    
#     if (length(addedArgs <- list(...)) && !(addedArgs %in% names(formals(getS3method("tokenize", "character")))))
#         warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")

    if (verbose && grepl("^dfm\\.character", sys.calls()[[2]]))
        catm("Creating a dfm from a character vector ...")

    # case conversion and tokenization
    # includes bigram tokenization but this will be merged soon into tokenize()
    if (toLower) {
        if (verbose) catm("\n   ... lowercasing", sep="")
        x <- toLower(x)
    }
    
    if (verbose) catm("\n   ... tokenizing", sep="")
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
    
    dfm(tokenizedTexts, verbose=verbose, toLower = FALSE, stem=stem, 
        ignoredFeatures=ignoredFeatures, keptFeatures = keptFeatures,
        language=language,
        thesaurus=thesaurus, dictionary=dictionary, valuetype = valuetype, 
        startTime = startTime)
}

    
#' @rdname dfm
#' @importFrom utils glob2rx
#' @export
dfm.tokenizedTexts <- function(x, 
                               verbose = TRUE,
                               toLower = FALSE,
                               stem = FALSE, 
                               ignoredFeatures = NULL, 
                               keptFeatures = NULL,
                               language = "english",
                               thesaurus = NULL, 
                               dictionary = NULL, 
                               valuetype = c("glob", "regex", "fixed"),
                               ...) {
    
    settings_ngrams <- attr(x, "ngrams")
    settings_concatenator <- attr(x, "concatenator")
    
    valuetype <- match.arg(valuetype)
    dots <- list(...)
    if (length(dots) && any(!(names(dots)) %in% c("startTime", "codeType")))
        warning("Argument", ifelse(length(dots)>1, "s ", " "), names(dots), " not used.", sep = "", noBreaks. = TRUE)
    
    startTime <- proc.time()
    if ("startTime" %in% names(dots)) startTime <- dots$startTime
    
    # argument checking
    language <- tolower(language)
    
    if (verbose && grepl("^dfm\\.tokenizedTexts", sys.calls()[[2]])) {
        catm("Creating a dfm from a tokenizedTexts object ...")
    }

    # lowercase if necessary 
    if (toLower) {
        if (verbose) catm("\n   ... lowercasing", sep="")
        x <- toLower(x)
    }

    # get document names
    if (is.null(names(x))) {
        docNames <- paste("text", 1:length(x), sep="")
    } else docNames <- names(x)
    
    # index documents
    if (verbose) catm("\n   ... indexing documents: ", 
                     format(length(x), big.mark=","), " document",
                     ifelse(length(x) > 1, "s", ""), sep="")
    nTokens <- lengths(x)
    # find out which documents have zero feature counts
    emptyDocs <- which(nTokens == 0)
    # add docIndex positions for any zero-token docs; no effect if emptyDocs is empty
    docIndex <- c(rep(seq_along(nTokens), nTokens), emptyDocs)

    # index features
    if (verbose) catm("\n   ... indexing features: ")
    if (sum(nTokens) == 0) {
        catm("\n   ... Error in dfm.tokenizedTexts(): no features found.\n")
        return(NULL)
    }
    allFeatures <- unlist(x)
    uniqueFeatures <- unique(allFeatures)
    totalfeatures <- length(uniqueFeatures)
    if (verbose) catm(format(totalfeatures - 1, big.mark=","), " feature type",
                     ifelse(totalfeatures - 1  > 1, "s", ""), sep="")
    featureIndex <- match(allFeatures, uniqueFeatures)
    # add an arbitrary "feature" for empty docs
    if (length(emptyDocs)) {
        featureIndex <- c(featureIndex, rep(length(uniqueFeatures)+1, length(emptyDocs)))
        uniqueFeatures <- c(uniqueFeatures, "__TEMPFEATURE__")
    }
    
    if (verbose) catm("\n")
    
    # make the dfm
    dfmresult <- sparseMatrix(i = docIndex, 
                              j = featureIndex, 
                              x = 1L, 
                              dimnames = list(docs = docNames, features = uniqueFeatures))
    # remove dummy feature if needed
    if (length(emptyDocs)) dfmresult <- dfmresult[, -ncol(dfmresult), drop = FALSE]
    # construct the dfmSparse type object
    dfmresult <- new("dfmSparse", dfmresult)
    
    # copy attributes
    dfmresult@ngrams <- settings_ngrams
    dfmresult@concatenator <- settings_concatenator
    
    if (!is.null(dictionary) | !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- thesaurus
        if (verbose) catm("   ... ")
        dfmresult <- applyDictionary(dfmresult, dictionary,
                                     exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                                     valuetype = valuetype,
                                     verbose = verbose)
    }
    
    if (!is.null(ignoredFeatures)) {
        if (verbose) catm("   ... ")
        dfmresult <- selectFeatures(dfmresult, ignoredFeatures, selection = "remove", valuetype = valuetype, verbose = verbose)
    }
    
    if (!is.null(keptFeatures)) {
        if (verbose) catm("   ... ")
        dfmresult <- selectFeatures(dfmresult, keptFeatures, selection = "keep", valuetype = valuetype, verbose = verbose)
    }
    
    if (stem) {
        if (verbose) catm("   ... stemming features (", stri_trans_totitle(language), ")", sep="")
        oldNfeature <- nfeature(dfmresult)
        dfmresult <- wordstem(dfmresult, language)
        if (verbose) 
            if (oldNfeature - nfeature(dfmresult) > 0) 
                catm(", trimmed ", oldNfeature - nfeature(dfmresult), " feature variant",
                    ifelse(oldNfeature - nfeature(dfmresult) != 1, "s", ""), "\n", sep = "")
        else
            catm("\n")
    }
    
    if (verbose) 
        catm("   ... created a", paste(dim(dfmresult), collapse=" x "), 
            "sparse dfm\n   ... complete. \nElapsed time:", (proc.time() - startTime)[3], "seconds.\n")
    
    # remove any NA named columns
    if (any(naFeatures <- is.na(features(dfmresult))))
        dfmresult <- dfmresult[, -which(naFeatures), drop = FALSE]
    
    return(dfmresult)
}

    

#' @rdname dfm
#' @param groups character vector containing the names of document variables for
#'   aggregating documents
#' @export
dfm.corpus <- function(x, verbose = TRUE, groups = NULL, ...) {
    if (verbose) catm("Creating a dfm from a corpus ...")
    
    if (!is.null(groups)) {
        groupsLab <- ifelse(is.factor(groups), deparse(substitute(groups)), groups)
        if (verbose) catm("\n   ... grouping texts by variable", 
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


