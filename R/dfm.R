#### dfm working and construction functions
####
#### Ken Benoit

#' create a document-feature matrix
#' 
#' Construct a sparse document-feature matrix, from a character, \link{corpus}, 
#' or \link{tokens} object.
#' @param x character, corpus, or tokens object
#' @param tolower convert all tokens to lowercase
#' @param stem if \code{TRUE}, stem words
#' @param remove a character vector of user-supplied features to ignore, such as
#'   "stop words".  To access one possible list (from any list you wish), use
#'   \code{\link{stopwords}()}.  The pattern matching type will be set by
#'   \code{valuetype}.  For behaviour of \code{remove} with \code{ngrams > 1},
#'   see Details.
#' @param select a user supplied regular expression defining which features to
#'   keep, while excluding all others.  This can be used in lieu of a dictionary
#'   if there are only specific features that a user wishes to keep. To extract
#'   only Twitter usernames, for example, set \code{select = "@@*"} and make
#'   sure that \code{removeTwitter = FALSE} as an additional argument passed to
#'   \link{tokenize}.  Note: \code{select = "^@@\\\w+\\\b"} would be the regular
#'   expression version of this matching pattern.  The pattern matching type
#'   will be set by \code{valuetype}.
#' @param dictionary A list of character vector dictionary entries, including 
#'   regular expressions (see examples)
#' @param thesaurus A list of character vector "thesaurus" entries, in a 
#'   dictionary list format, which operates as a dictionary but without 
#'   excluding values not matched from the dictionary.  Thesaurus keys are 
#'   converted to upper case to create a feature label in the dfm, as a reminder
#'   that this was not a type found in the text, but rather the label of a 
#'   thesaurus key.  For more fine-grained control over this and other aspects 
#'   of converting features into dictionary/thesaurus keys from pattern matches 
#'   to values, you can use \code{\link{dfm_lookup}} after creating the 
#'   dfm.
#' @param valuetype \code{fixed} for words as is; \code{"regex"} for regular 
#'   expressions; or \code{"glob"} for "glob"-style wildcard; \code{"glob"}
#'   format is the default.  See \code{\link{dfm_select}}.
#' @param groups character vector containing the names of document variables for
#'   aggregating documents; only applies when calling dfm on a corpus object
#' @param verbose display messages if \code{TRUE}
#' @param ... additional arguments passed to \link{tokens}, for character and
#'   corpus
#' @details The default behavior for \code{remove}/\code{select} when
#'   constructing ngrams using \code{dfm(x, } \emph{ngrams > 1}\code{)} is to
#'   remove/select any ngram constructed from a matching feature.  If you wish
#'   to remove these before constructing ngrams, you will need to first tokenize
#'   the texts with ngrams, then remove the features to be ignored, and then 
#'   construct the dfm using this modified tokenization object.  See the code 
#'   examples for an illustration.
#' @return a \link{dfm-class} object
#' @import Matrix
#' @export
#' @name dfm
#' @keywords dfm
#' @examples
#' ## for a corpus
#' 
#' corpus_post80inaug <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' dfm(corpus_post80inaug)
#' dfm(corpus_post80inaug, tolower = FALSE)
#' 
#' # grouping documents by docvars in a corpus
#' dfm(corpus_post80inaug, groups = "President", verbose = TRUE)
#' 
#' # with English stopwords and stemming
#' dfm(corpus_post80inaug, remove = stopwords("english"), stem = TRUE, verbose = TRUE)
#' # works for both words in ngrams too
#' dfm("Banking industry", stem = TRUE, ngrams = 2, verbose = FALSE)
#' 
#' # with dictionaries
#' corpus_post1900inaug <- corpus_subset(data_corpus_inaugural, Year>1900)
#' mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                opposition = c("Opposition", "reject", "notincorpus"),
#'                taxing = "taxing",
#'                taxation = "taxation",
#'                taxregex = "tax*",
#'                country = "united states"))
#' dfm(corpus_post1900inaug, dictionary = mydict)
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
#' featnames(dfm(testCorpus, select = stopwords("english")))
#' # for ngrams
#' featnames(dfm(testCorpus, ngrams = 2, select = stopwords("english"), removePunct = TRUE))
#' featnames(dfm(testCorpus, ngrams = 1:2, select = stopwords("english"), removePunct = TRUE))
#' 
#' ## removing stopwords before constructing ngrams
#' tokensAll <- tokens(toLower(testText), removePunct = TRUE)
#' tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
#' tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 2)
#' featnames(dfm(tokensNgramsNoStopwords, verbose = FALSE))
#' 
#' # keep only certain words
#' dfm(testCorpus, select = "*s", verbose = FALSE)  # keep only words ending in "s"
#' dfm(testCorpus, select = "s$", valuetype = "regex", verbose = FALSE)
#' 
#' # testing Twitter functions
#' testTweets <- c("My homie @@justinbieber #justinbieber shopping in #LA yesterday #beliebers",
#'                 "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(testTweets, select = "#*", removeTwitter = FALSE)  # keep only hashtags
#' dfm(testTweets, select = "^#.*$", valuetype = "regex", removeTwitter = FALSE)
dfm <- function(x, 
                tolower = TRUE,
                stem = FALSE,
                select = NULL,
                remove = NULL,
                thesaurus = NULL,
                dictionary = NULL,
                valuetype = c("glob", "regex", "fixed"), 
                groups = NULL, 
                verbose = FALSE, ...) {

    UseMethod("dfm")
}

#' @noRd
#' @author Kenneth Benoit
#' @import Matrix
#' @export
dfm.character <- function(x, 
                tolower = TRUE,
                stem = FALSE,
                select = NULL,
                remove = NULL,
                thesaurus = NULL,
                dictionary = NULL,
                valuetype = c("glob", "regex", "fixed"), 
                groups = NULL, 
                verbose = FALSE, ...) {
    startTime <- proc.time()
    valuetype <- match.arg(valuetype)
    
    if (verbose && grepl("^dfm\\.character", sys.calls()[2]))
        catm("Creating a dfm from a character vector ...")

    if (tolower) {
        if (verbose) catm("\n   ... lowercasing", sep="")
        x <- toLower(x)
    }
    
    if (verbose) catm("\n   ... tokenizing", sep = "")
    tokenizedTexts <- tokens(x, ...)

    dfm(tokenizedTexts, verbose = verbose, tolower = FALSE, stem = stem, 
        remove = remove, select = select,
        thesaurus = thesaurus, dictionary = dictionary, valuetype = valuetype, 
        startTime = startTime)
}


#' @noRd
#' @export
dfm.corpus <- function(x, tolower = TRUE,
                       stem = FALSE,
                       select = NULL,
                       remove = NULL,
                       thesaurus = NULL,
                       dictionary = NULL,
                       valuetype = c("glob", "regex", "fixed"), 
                       groups = NULL, 
                       verbose = FALSE, ...) {
    if (verbose) {
        catm("Creating a dfm from a corpus ...")
        if (!is.null(groups)) {
            groupsLab <- ifelse(is.factor(groups), deparse(substitute(groups)), groups)
            catm("\n   ... grouping texts by variable", 
                 ifelse(length(groupsLab) == 1, "", "s"), ": ", 
                 paste(groupsLab, collapse=", "), sep="")
        }
    }
    dfm(texts(x, groups = groups),  tolower = tolower,
        stem = stem,
        select = select,
        remove = remove,
        thesaurus = thesaurus,
        dictionary = dictionary,
        valuetype = valuetype, 
        verbose = verbose, ...)
}    

    
#' @noRd
#' @importFrom utils glob2rx
#' @export
dfm.tokenizedTexts <- function(x, 
                               tolower = TRUE,
                               stem = FALSE, 
                               select = NULL,
                               remove = NULL,
                               thesaurus = NULL,
                               dictionary = NULL,
                               valuetype = c("glob", "regex", "fixed"), 
                               groups = NULL, 
                               verbose = FALSE, ...) {
    
    settings_ngrams <- attr(x, "ngrams")
    settings_skip <- attr(x, "skip")
    settings_concatenator <- attr(x, "concatenator")
    
    valuetype <- match.arg(valuetype)
    dots <- list(...)
    if (length(dots) && any(!(names(dots)) %in% c("startTime", "codeType")))
        warning("Argument", ifelse(length(dots)>1, "s ", " "), names(dots), " not used.", sep = "", noBreaks. = TRUE)
    
    startTime <- proc.time()
    if ("startTime" %in% names(dots)) startTime <- dots$startTime
    
    if (verbose & stri_startswith_fixed(sys.calls()[2], "dfm.token"))
        if (tolower) {
            if (verbose) catm("\n   ... lowercasing", sep="")
            x <- toLower(x)
        }
    
    # set document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    } 
    
    # compile the dfm
    dfmresult <- compile_dfm(x, verbose = verbose)
    
    # copy attributes
    dfmresult@ngrams <- as.integer(settings_ngrams)
    dfmresult@skip <- as.integer(settings_skip)
    dfmresult@concatenator <- settings_concatenator
    
    if (!is.null(dictionary) | !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- thesaurus
        if (verbose) catm("   ... ")
        dfmresult <- dfm_lookup(dfmresult, dictionary,
                                exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                                valuetype = valuetype,
                                verbose = verbose)
    }
    
    if (!is.null(remove)) {
        if (verbose) catm("   ... ")
        # if ngrams > 1 and remove or selct is specified, then convert these into a
        # regex that will remove any ngram containing one of the words
        if (!identical(settings_ngrams, 1L)) {
            remove <- make_ngram_pattern(remove, valuetype, settings_concatenator)
            valuetype <- "regex"
        }
        dfmresult <- dfm_select(dfmresult, remove, selection = "remove", 
                                valuetype = valuetype, verbose = verbose)
    }
    
    if (!is.null(select)) {
        if (verbose) catm("   ... ")
        # if ngrams > 1 and remove or selct is specified, then convert these into a
        # regex that will remove any ngram containing one of the words
        if (!identical(settings_ngrams, 1L)) {
            select <- make_ngram_pattern(select, valuetype, settings_concatenator)
            valuetype <- "regex"
        }
        dfmresult <- dfm_select(dfmresult, select, selection = "keep", 
                                valuetype = valuetype, verbose = verbose)
    }
    
    language <- "english"
    if (stem) {
        if (verbose) catm("   ... stemming features (", stri_trans_totitle(language), ")", sep="")
        oldNfeature <- nfeature(dfmresult)
        dfmresult <- dfm_wordstem(dfmresult, language)
        if (verbose) 
            if (oldNfeature - nfeature(dfmresult) > 0) 
                catm(", trimmed ", oldNfeature - nfeature(dfmresult), " feature variant",
                     ifelse(oldNfeature - nfeature(dfmresult) != 1, "s", ""), "\n", sep = "")
        else
            catm("\n")
    }
    
    if (verbose) 
        catm("   ... created a", paste(format(dim(dfmresult), big.mark=",", trim = TRUE), 
                                       collapse=" x "), 
             "sparse dfm\n   ... complete. \nElapsed time:", 
             format((proc.time() - startTime)[3], digits = 3),
             "seconds.\n")
    
    # remove any NA named columns
    if (any(naFeatures <- is.na(featnames(dfmresult))))
        dfmresult <- dfmresult[, -which(naFeatures), drop = FALSE]
    
    return(dfmresult)
}



####
#### core constructors for dfm 
####

## internal function to compile the dfm
compile_dfm <- function(x, verbose = TRUE) {
    UseMethod("compile_dfm")
}

## internal function to compile the dfm
compile_dfm.tokenizedTexts <- function(x, verbose = TRUE) {

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
    allFeatures <- unlist(x, use.names=FALSE)
    uniqueFeatures <- unique(allFeatures)
    totalfeatures <- length(uniqueFeatures)
    if (verbose) catm(format(totalfeatures, big.mark=","), " feature type",
                      ifelse(totalfeatures > 1, "s", ""), sep="")
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
                              dimnames = list(docs = names(x), 
                                              features = uniqueFeatures))
    # remove dummy feature if needed
    if (length(emptyDocs)) dfmresult <- dfmresult[, -ncol(dfmresult), drop = FALSE]
    
    new("dfmSparse", dfmresult)
}

compile_dfm.tokens <- function(x, verbose = TRUE) {
    
    if (verbose) {
        catm("\n   ... found ", 
             format(length(x), big.mark = ","), " document",
             ifelse(length(x) > 1, "s", ""), ### replace with: ntoken()
             ", ",
             format(length(types(x)), big.mark = ","),  ### replace with: ntype()
             " feature",
             ifelse(length(types(x)) > 1, "s", ""),
             "\n",
             sep="")
    }

    ## Special handling for empty documents
    # find out which documents have zero feature counts
    types <- types(x)
    x <- unclass(x)
    emptyDocs <- which(lengths(x) == 0)
    # Add an arbitrary "feature" for empty docs
    if (length(emptyDocs)) {
        x[emptyDocs] <- length(types) + 1
        types <- c(types, "__TEMPFEATURE__")
    }

    dfmresult <- t(Matrix::sparseMatrix(i = unlist(x, use.names = FALSE), 
                                        p = cumsum(c(1, lengths(x))) - 1, 
                                        x = 1L, 
                                        dimnames = list(features = types, 
                                                        docs = names(x))))
    # Remove dummy feature if needed
    if (length(emptyDocs)) dfmresult <- dfmresult[, -ncol(dfmresult), drop = FALSE]
    gc() # Release memory
    new("dfmSparse", dfmresult)
}


####
#### utility functions
####

## convert patterns (remove and select) to ngram regular expressions
make_ngram_pattern <- function(features, valuetype, concatenator) {
    if (valuetype == "glob") {
        features <- gsub("\\*", ".*", features)
        features <- gsub("\\?", ".{1}", features)
    }
    features <- paste0("(\\b|(\\w+", concatenator, ")+)", 
                       features, "(\\b|(", concatenator, "\\w+)+)")
    features
}

