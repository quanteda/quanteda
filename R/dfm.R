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
#' @param addto \code{NULL} by default, but if an existing dfm object is specified, then the new dfm will be added to the one named.
#' If both dfms are built from dictionaries, the combined dfm will have its \code{Non_Dictionary} total adjusted.
#' @return A matrix object with row names equal to the document names and column names equal to the feature labels.  
#' This matrix has \code{names(dimnames) = c("docs", "words")}
#' to make it conformable to an \link[austin]{wfm} object.
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
#' dictDfm
#' 
#' ## adding one dfm to another
#' mydict2 <- list(partyref=c("Lenihan", "Fianna", "Sinn", "Gael"))
#' dictDfm2 <- dfm(corpus, dictionary=mydict2, addto=dictDfm)
#' dictDfm2
dfm <- function(corpus,
                feature=c("word"),
                stem=FALSE,
                stopwords=FALSE,
                bigram=FALSE,
                groups=NULL,
                subset=NULL, 
                verbose=TRUE, 
                dictionary=NULL,
                dictionary.regex=FALSE,
                addto=NULL) {
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
                       dictionary.regex=FALSE,
                       addto=NULL) {
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
        if (verbose) cat("complete ...")
    } else {
        texts <- corpus$attribs$texts
        names(texts) <- rownames(corpus$attribs)
    }
    
    textnames <- factor(names(texts))
    tokenizedTexts <- sapply(texts, tokenize, simplify=FALSE)
    if (stem==TRUE) {
        require(SnowballC)
        cat(" stemming ...")
        tokenizedTexts <- lapply(tokenizedTexts, wordStem)
    }
    if (bigram > 0) {
        cat(" making bigrams ...")
        tokenizedTexts <- lapply(tokenizedTexts, function(x) bigrams(x, bigram))
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
    
    if (stopwords) {
        cat(" removing stopwords ...")
        data(stopwords_EN)
        if (bigram==TRUE) {
          pat <- paste(paste0(paste0("-", stopwords_EN, "$"), collapse='|'), paste0(paste0("^", stopwords_EN, "-"), collapse='|'), sep='|')
          dfm <- t(subset(t(dfm), !grepl(pat, colnames(dfm))))
        } else {
          dfm <- t(subset(t(dfm), !colnames(dfm) %in% stopwords_EN))
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
    
    # give the matrix austin a "wfm"-like record of which margin is words, which is docs
    dfm <- as.matrix(dfm)
    dimnames(dfm) <- list(docs = rownames(dfm), words = colnames(dfm))
    
    if(verbose) cat(" done. \n")
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

#' Trim a dfm based on a subset of features and words
#'
#' Returns a document by feature matrix reduced in size based on document and term frequency, and/or subsampling.
#' 
#' @param dfm Document-feature matrix created by \code{\link{dfm}}
#' @param min.count minimum feature count
#' @param min.doc minimum number of documents in which a feature appears
#' @param sample how many features to retain (based on random selection)
#' @return A dfm matrix object reduced in size.
#' @export 
#' @author Will Lowe, adapted by Ken Benoit
#' @examples 
#' data(iebudgets)
#' dtm <- dfm(iebudgets)
#' dim(dtm)  # 196 docs x 13343 words
#' dtm.reduced <- dfmTrim(dtm, min.count=10, min.doc=3) # only words occuring at least 10 times and in at least 3 documents
#' dim(dtm.reduced)  # 196 docs x 3006 words
#' dtm.sampled <- dfmTrim(dtm, sample=200)  # top 200 words
#' dim(dtm.sampled)  # 196 x 200 words
dfmTrim <- function(dfm, min.count=5, min.doc=5, sample=NULL, verbose=TRUE) {
    nms <- names(dimnames(dfm))
    if (!(!is.null(nms) && identical(sort(nms), c("docs", "words")))) 
        stop("Function not applicable to this object")

    mY <- dfm
    if (names(dimnames(dfm))[2] == "words") 
        mY <- t(mY)
    
    rs1 <- which(rowSums(mY) >= min.count)
    if (verbose)
        cat("Words appearing less than", min.count, "times:", (nrow(mY) - length(rs1)), "\n")
    
    rs2 <- which(apply(mY, 1, function(x){ sum(x>0) >= min.doc } ))
    if (verbose)
        cat("Words appearing in fewer than", min.doc, "documents:", (nrow(mY) - length(rs2)), "\n")
    
    tokeep <- intersect(rs1, rs2)
    if (length(tokeep)==0)
        stop("No words left after trimming")
    
    if (!is.null(sample)) {
        if (sample > length(tokeep))
            warning(paste('Sample size', sample, 'larger than',
                          length(tokeep), "already filtered from", nrow(mY), "so ignoring sampling request"))
        tokeep <- sample(tokeep, min(length(tokeep), sample))
        if (verbose)
            cat("Retaining a random sample of", sample, "words\n")
    }
    return(t(mY[sort(tokeep),]))
}

    
