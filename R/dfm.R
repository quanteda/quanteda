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
#' @param dictionary A list of character vector dictionary entries, including regular expressions (see examples) 
#' @param dictionary.regex \code{TRUE} means the dictionary is already in regular expression format,
#' otherwise it will be converted from "wildcard" format
#' @return A data frame with row names equal to the document names and column names equal to the feature labels.
#' @rdname dfm
#' @export 
#' @author Kenneth Benoit
#' @examples 
#' data(iebudgets)
#' wfm <- dfm(iebudgets)
#' 
#' ## by party, subset for 2010
#' wfmByParty2010 <- dfm(subset(iebudgets, year==2010), groups="party")
#' 
#' ## with dictionaries
#' corpus <- subset(iebudgets, year==2010)
#' mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
#'                opposition=c("Opposition", "reject", "notincorpus"),
#'                taxing="taxing",
#'                taxation="taxation",
#'                taxregex="tax*")
#' dictDfm <- dfm(corpus, dictionary=mydict)
dfm <- function(corpus,
                feature=c("word"),
                stem=FALSE,
                stopwords=FALSE,
                groups=NULL,
                subset=NULL, 
                verbose=TRUE, 
                dictionary=NULL,
                dictionary.regex=FALSE) {
    UseMethod("dfm")
}

#' @rdname dfm
#' @method dfm corpus
#' @S3method dfm corpus
dfm.corpus <- function(corpus,
                       feature=c("word"),
                       stem=FALSE,
                       stopwords=FALSE,
                       bigram=FALSE,
                       groups=NULL,
                       subset=NULL, 
                       verbose=TRUE, 
                       dictionary=NULL,
                       dictionary.regex=FALSE) {
    # require(austin) 
    # --not necessary to call austin if not returning a wfm class object
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
        cat("... stemming ...")
        tokenizedTexts <- lapply(tokenizedTexts, wordStem)
    }
    if(bigram==TRUE) {
        tokenizedTexts <- lapply(tokenizedTexts, bigrams)
    }
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
        if (!dictionary.regex) {
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
    
    if(verbose) cat(" done. \n")
    
    if (stopwords) {
        data(stopwords_EN)
        dfm <- subset(dfm, !row.names(dfm) %in% stopwords_EN)
    }
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

#dictionary
#lapply(dictionary, makeRegEx)


