#' select features from an object
#' 
#' This function selects or discards features from a dfm.variety of objects, 
#' such as tokenized texts, a dfm, or a list of collocations.  The most common 
#' usage for \code{removeFeatures} will be to eliminate stop words from a text 
#' or text-based object, or to select only features from a list of regular 
#' expression.
#' @param x object whose features will be selected
#' @param features one of: a character vector of features to be selected, a 
#'   \link{dfm} whose features will be used for selection, or a dictionary class
#'   object whose values (not keys) will provide the features to be selected. 
#'   For \link{dfm} objects, see details in the Value section below.
#' @param selection whether to keep or remove the features
#' @param valuetype how to interpret feature vector: \code{fixed} for words as 
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for 
#'   "glob"-style wildcard
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param verbose if \code{TRUE} print message about how many features were 
#'   removed
#' @param ... supplementary arguments passed to the underlying functions in 
#'   \code{\link[stringi]{stri_detect_regex}}.  (This is how 
#'   \code{case_insensitive} is passed, but you may wish to pass others.)
#' @note This function selects features based on their labels.  To select 
#'   features based on the values of a the document-feature matrix, use 
#'   \code{\link{trim}}.
#' @return A dfm after the feature selection has been applied.
#'   
#'   When \code{features} is a \link{dfm-class} object, then the returned object
#'   will be identical in its feature set to the dfm supplied as the
#'   \code{features} argument.  This means that any features in \code{x} not in
#'   \code{features} will be discarded, and that any features in found in the
#'   dfm supplied as \code{features} but not found in \code{x} will be added
#'   with all zero counts.  This is useful when you have trained a model on one dfm, and
#'   need to project this onto a test set whose features must be identical.
#'   
#' @export
#' @seealso \code{\link{removeFeatures}}, \code{\link{trim}}
#' @examples 
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              toLower = FALSE, verbose = FALSE)
#' mydict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
#'                           wordsEndingInY = c("by", "my"),
#'                           notintext = "blahblah"))
#' selectFeatures(myDfm, mydict)
#' selectFeatures(myDfm, mydict, case_insensitive = FALSE)
#' selectFeatures(myDfm, c("s$", ".y"), "keep")
#' selectFeatures(myDfm, c("s$", ".y"), "keep", valuetype = "regex")
#' selectFeatures(myDfm, c("s$", ".y"), "remove", valuetype = "regex")
#' selectFeatures(myDfm, stopwords("english"), "keep", valuetype = "fixed")
#' selectFeatures(myDfm, stopwords("english"), "remove", valuetype = "fixed")
#' 
#' # selecting on a dfm
#' textVec1 <- c("This is text one.", "This, the second text.", "Here: the third text.")
#' textVec2 <- c("Here are new words.", "New words in this text.")
#' (dfm1 <- dfm(textVec1, verbose = FALSE))
#' (dfm2a <- dfm(textVec2, verbose = FALSE))
#' (dfm2b <- selectFeatures(dfm2a, dfm1))
#' setequal(features(dfm1), features(dfm2b))
#' 
#' # more selection on a dfm
#' selectFeatures(dfm1, dfm2a)
#' selectFeatures(dfm1, dfm2a, selection = "remove")
selectFeatures <- function(x, features, ...) {
    UseMethod("selectFeatures")
}


#' @rdname selectFeatures
#' @export
selectFeatures.dfm <- function(x, features, selection = c("keep", "remove"), 
                               valuetype = c("glob", "regex", "fixed"),
                               case_insensitive = TRUE,
                               verbose = TRUE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features_from_dfm <- FALSE
    if (!(is.character(features) | is.dfm(features) | is(features, "dictionary")))
        stop("features must be of type character, dictionary, or dfm")
    if (is.dfm(features)) {
        if (selection == "keep") {
            features_dfm <- features <- features(features)
            features_from_dfm <- TRUE
        } else {
            features <- features(features)
        }
    }
    
    features <- unique(unlist(features))  # to convert any dictionaries
    
    originalvaluetype <- valuetype
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    features_x <- features(x)
    if (case_insensitive & valuetype == "fixed") {
        features_x <- toLower(features_x)
        features <- toLower(features)
    }
    # split features on concatenator if exists
    if (x@concatenator != "")
        features_x <- strsplit(features_x, x@concatenator)
    
    if (valuetype == "regex") {
        if (all.equal(x@ngrams, 1L)==TRUE) {
            featIndex <- which(stringi::stri_detect_regex(features_x, paste0(features, collapse = "|"), 
                                                          case_insensitive = case_insensitive, ...))
        } else {
            ####
            ####
            matchPattern <- paste0(features, collapse = "|")
            featIndex <- which(sapply(features_x, 
                                      function(x) any(stringi::stri_detect_regex(x, matchPattern, 
                                                                                 case_insensitive = case_insensitive, ...))))
        }
    } else {
        if (all.equal(x@ngrams, 1L)==TRUE)
            featIndex <- which(features(x) %in% features)  # unigrams
        else
            featIndex <- which(sapply(features_x, function(f) any(f %in% features), USE.NAMES = FALSE)) # ngrams
    }
    
    if (verbose & !features_from_dfm) 
        catm(ifelse(selection=="keep", "kept", "removed"), " ", 
            format(length(featIndex), big.mark=","),
            " feature", ifelse(length(featIndex) > 1 | length(featIndex)==0, "s", ""), 
            ", from ", length(features), " supplied (", originalvaluetype, ") feature type",
            ifelse(length(features) > 0 | length(featIndex)==0, "s", ""),
            "\n", sep = "")

    # pad the zeros if features was a dfm, return in same feature order as original dfm
    # for selection = "keep" only
    if (features_from_dfm) {
        
        if (verbose)
            catm(ifelse(selection=="keep", "found", "zeroed"), " ", 
                format(length(featIndex), big.mark=","),
                " feature", ifelse(length(featIndex) > 1 | length(featIndex)==0, "s", ""), 
                " from ", length(features), " supplied type",
                ifelse(length(features) > 0 | length(featIndex)==0, "s", ""),
                " in a dfm,", sep = "")
        
        
        # remove features in x that are not in features (from supplied dfm)
        x2 <- x[, featIndex]
        # now add zero-valued features to x that are not in x but are in features
        origDfmFeatureIndex <- which(!(toLower(features) %in% toLower(features(x2))))
        xOriginalFeatureLength <- nfeature(x2)
        xOriginalFeatures <- features(x2)
        ### NEED a cbind() operation for dfm that preserves settings! ###
        if (verbose) catm(" padding 0s for another", length(origDfmFeatureIndex), "\n")
        x <- new("dfmSparse", Matrix::cbind2(x2,
                                              sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(x2), length(origDfmFeatureIndex)), 
                                                           dimnames = list(docnames(x2), features[origDfmFeatureIndex]))))
#                  matrix(0, nrow = ndoc(x2), ncol = length(origDfmFeatureIndex))))
#        x <- cbind(x2, )
#        colnames(x2)[(xOriginalFeatureLength + 1) : nfeature(x2)] <- features[origDfmFeatureIndex]
        featIndex <- match(features_dfm, features(x))
        # x <- x2 #[, features_dfm]
    }
    
    ##
    ## MIGHT NEED TO ADD BACK ORIGINAL ATTRIBUTES HERE
    ##
    
    if (!length(featIndex)) 
        return(x)
    
    if (selection == "keep")
        return(x[, featIndex])
    else 
        return(x[, -featIndex])
}

#' @rdname selectFeatures
#' @param padding (only for \code{tokenizedTexts} objects) if \code{TRUE}, leave
#'   an empty string where the removed tokens previously existed.  This is
#'   useful if a positional match is needed between the pre- and post-selected
#'   features, for instance if a window of adjacency needs to be computed.
#' @param indexing use dfm-based index to efficiently process large tokenizedTexts object
#' @export
#' @examples 
#' \dontrun{## performance comparisons
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' # toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' # head to head, old v. new
#' system.time(selectFeaturesOLD(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeaturesOLD(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' system.time(selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' microbenchmark::microbenchmark(
#'     old = selectFeaturesOLD(toks, stopwords("english"), "remove", verbose = FALSE),
#'     new = selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE),
#'     times = 5, unit = "relative")
#' microbenchmark::microbenchmark(
#'     new = selectFeaturesOLD(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     old = selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     times = 2, unit = "relative")
#'     
#' types <- unique(unlist(toks))
#' numbers <- types[stringi::stri_detect_regex(types, '[0-9]')]
#' microbenchmark::microbenchmark(
#'     new = selectFeaturesOLD(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     old = selectFeatures(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     times = 2, unit = "relative")  
#'     
#' # removing tokens before dfm, versus after
#' microbenchmark::microbenchmark(
#'     pre = dfm(selectFeaturesOLD(toks, stopwords("english"), "remove"), verbose = FALSE),
#'     post = dfm(toks, ignoredFeatures = stopwords("english"), verbose = FALSE),
#'     times = 5, unit = "relative")
#' }
#' 
#' ## with simple examples
#' toks <- tokenize(c("This is a sentence.", "This is a second sentence."), 
#'                  removePunct = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' 
#' # how case_insensitive works
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = FALSE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = FALSE)
#' 
#' # with longer texts
#' txts <- c(exampleString, inaugTexts[2])
#' toks <- tokenize(txts)
#' selectFeatures(toks, stopwords("english"), "remove")
#' selectFeatures(toks, stopwords("english"), "keep")
#' selectFeatures(toks, stopwords("english"), "remove", padding = TRUE)
#' selectFeatures(toks, stopwords("english"), "keep", padding = TRUE)
#' selectFeatures(tokenize(encodedTexts[1]), stopwords("english"), "remove", padding = TRUE)
selectFeatures.tokenizedTexts <- function(x, features, selection = c("keep", "remove"), 
                                           valuetype = c("glob", "regex", "fixed"),
                                           case_insensitive = TRUE, padding = FALSE, indexing = FALSE,
                                           verbose = FALSE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    originalvaluetype <- valuetype
    features <- unique(unlist(features, use.names=FALSE))  # to convert any dictionaries
    y <- deepcopy(x) # copy x to y to prevent changes in x
    n <- length(y)
    
    if(indexing){
        if(verbose) catm("Indexing tokens...\n")
        index <- dfm(y, verbose = FALSE)
        index_binary <- as(index, 'nMatrix')
        types <- colnames(index_binary)
    }else{
        types <- unique(unlist(y, use.names=FALSE))
        flag <- rep(TRUE, n)
    }
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {
        
        if (case_insensitive) {
            #types <- unique(unlist(y, use.names=FALSE))
            types_match <- types[toLower(types) %in% toLower(features)]
        } else {
            types_match <- features
        }
        if (indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if (verbose) catm(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if(selection == "remove"){
            select_tokens_cppl(y, flag, types_match, TRUE, padding)
        }else{ 
            select_tokens_cppl(y, flag, types_match, FALSE, padding)
        }
    } else if (valuetype == "regex") {
        if (verbose) catm("Converting regex to fixed...\n")
        types_match <- regex2fixed(features, types, case_insensitive) # get all the unique types that match regex
        if(indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if(verbose) catm(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if (selection == "remove") {
            select_tokens_cppl(y, flag, types_match, TRUE, padding)  # search as fixed
        } else {
            select_tokens_cppl(y, flag, types_match, FALSE, padding) # search as fixed
        }
    }
    
    class(y) <- c("tokenizedTexts", class(x))
    attributes(y) <- attributes(x)
    return(y)
}

#' @rdname selectFeatures
#' @param pos indexes of word position if called on collocations: remove if word
#'   \code{pos} is a stopword
#' @examples  
#' 
#' ## example for collocations
#' (myCollocs <- collocations(inaugTexts[1:3], n=20))
#' selectFeatures(myCollocs, stopwords("english"), "remove")
#' @export
selectFeatures.collocations <- function(x, features, selection = c("keep", "remove"), 
                                        valuetype = c("glob", "regex", "fixed"),
                                        case_insensitive = TRUE,
                                        verbose = TRUE, pos = 1:3, ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features <- unique(unlist(features))  # to convert any dictionaries
    if (case_insensitive) features <- toLower(features)
    
    if (valuetype == "regex")
        stop("regex not currently supported for selectFeatures.collocations")
    
    if (selection == "keep")
        stop("keep not currently supported for selectFeatures.collocations")
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            stop("glob not currently supported for selectFeatures.collocations")
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    word <- word1 <- word2 <- word3 <- NULL
    origclass <- class(x)
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    nstart <- nrow(x)
    stopwordTable <- data.table(word = features, remove = 1L)
    setkey(stopwordTable, word)
    x$order <- 1:nrow(x)
    
    if (case_insensitive)
        x[, c("word1", "word2", "word3") := list(toLower(word1), toLower(word2), toLower(word3))]
    
    if (3 %in% pos) {
        setnames(stopwordTable, 1, "word3")
        setkey(x, word3)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (2 %in% pos) {
        setnames(stopwordTable, 1, "word2")
        setkey(x, word2)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (1 %in% pos) {
        setnames(stopwordTable, 1, "word1")
        setkey(x, word1)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    setorder(x, order)
    setcolorder(x, c("word1", "word2", "word3", names(x)[4:ncol(x)]))
    x[, order:=NULL]
    nend <- nrow(x)
    if (verbose) catm("Removed ", format(nstart - nend, big.mark=","),  
                     " (", format((nstart - nend)/nstart*100, digits=3),
                     "%) of ", format(nstart, big.mark=","), 
                     " collocations containing one of ", 
                     length(features), " stopwords.\n", sep="")
    class(x) <- origclass
    x
}




#' remove features from an object
#' 
#' Removes features from a variety of objects, such as text, a
#' dfm, or a list of collocations.  The most common usage for
#' \code{removeFeatures} will be to eliminate stop words from a text or
#' text-based object.  This function simply provides a convenience wrapper for
#' \code{\link{selectFeatures}} where \code{selection = "remove"}.
#' 
#' @param x object from which stopwords will be removed
#' @param features character vector of features to remove
#' @param ... additional arguments passed to \code{\link{selectFeatures}}
#' @return an object with matching features removed
#' @name removeFeatures
#' @export
#' @author Kenneth Benoit
#' @seealso \link{stopwords}
#' @examples
#' ## for tokenized texts 
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my country to 
#'                    execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall endeavor to express
#'                    the high sense I entertain of this distinguished honor.")
#' removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
#' 
#' itText <- tokenize("Ecco alcuni di testo contenente le parole che vogliamo rimuovere.", 
#'                    removePunct = TRUE)
#' removeFeatures(itText, stopwords("italian"), case_insensitive = TRUE)
#' 
#' ## example for dfm objects
#' mydfm <- dfm(ukimmigTexts, verbose=FALSE)
#' removeFeatures(mydfm, stopwords("english"))
#' 
#' ## example for collocations
#' (myCollocs <- collocations(inaugTexts[1:3], n=20))
#' removeFeatures(myCollocs, stopwords("english"))
#' removeFeatures(myCollocs, stopwords("english"), pos = 2)
removeFeatures <- function(x, features, ...) {
    if ("selection" %in% names(list(...)))
        stop("cannot override selection argument in removeFeatures")
    selectFeatures(x, features, selection = "remove", ...)
}


### WORKING/TESTING CODE #####################

# @rdname selectFeatures
# @examples
# ## example for collocations
# (myCollocs <- collocations(inaugTexts[1:3], n=20))
# selectFeatures(myCollocs, stopwords("english"), "remove")
# @export
selectFeatures_collocations <- function(x, features, selection = c("keep", "remove"), 
                                        valuetype = c("glob", "regex", "fixed"),
                                        case_insensitive = TRUE,
                                        verbose = TRUE, pos = 1:3, ...) {
    
    # necessary to pass by value - inefficient but "R-like"
    # xloc <- data.table::copy(x)
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features <- unique(unlist(features))  # to convert any dictionaries
    
    word <- word1 <- word2 <- word3 <- NULL
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {
        
        if (case_insensitive) {
            features <- toLower(features)
            x[, paste0("word", pos) := lapply(x[, pos, with = FALSE], tolower)]
        }            
        x[, paste0("i", pos) := lapply(x[, pos, with = FALSE], function(y) {
            if (selection == "remove") 
                !(y %in% features)
            else 
                (y %in% features)
        })]
        
    } else if (valuetype == "regex") {
        
        x[, paste0("i", pos) := lapply(x[, pos, with = FALSE], function(y) {
            if (selection == "remove") 
                !stringi::stri_detect_regex(y, paste0(features, collapse = "|"), case_insensitive = case_insensitive, ...)
            else 
                stringi::stri_detect_regex(y, paste0(features, collapse = "|"), case_insensitive = case_insensitive, ...)
        })]
        
    }
    
    x <- x[apply(x[, grep("^i", colnames(x)), with = FALSE], 1, all) == TRUE]
    x[, -(grep("^i", colnames(x))), with = FALSE]
}

# x <- data.table::copy(myCollocs)
# features <- stopwords("english")
# selection = c("remove") 
# valuetype = c("glob")
# case_insensitive = TRUE
# verbose = TRUE
# pos = 1:3

# require(microbenchmark)
# myCollocs <- collocations(inaugCorpus, size=2:3)
# microbenchmark(
#     old = removeFeatures(myCollocs, stopwords("english"), verbose = FALSE),
#                          new = selectFeatures(myCollocs, stopwords("english"), "remove"),
#                          unit = "relative", times = 30
#     )

# Convert regex to fixed
# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
# @param ... additional arguments passed to \code{\link[stringi]{stri_detect_regex}}
# @export
regex2fixed <- function(regex, types, case_insensitive = TRUE, ...){
    regex_joined <- paste0(regex, collapse = "|")
    types_match <- types[stringi::stri_detect_regex(types, regex_joined, case_insensitive = case_insensitive, ...)]  
    return(types_match)
}


