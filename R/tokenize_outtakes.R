## older ngram() function previously in tokenize.R

ngram <- function(tokens, n = 2, concatenator = "_", include.all = FALSE) {
    
    if (length(tokens) < n) 
        return(NULL)
    
    # start with lower ngrams, or just the specified size if include.all = FALSE
    start <- ifelse(include.all, 
                    1, 
                    ifelse(length(tokens) < n, 1, n))
    
    # set max size of ngram at max length of tokens
    end <- ifelse(length(tokens) < n, length(tokens), n)
    
    all_ngrams <- c()
    # outer loop for all ngrams down to 1
    for (width in start:end) {
        new_ngrams <- tokens[1:(length(tokens) - width + 1)]
        # inner loop for ngrams of width > 1
        if (width > 1) {
            for (i in 1:(width - 1)) 
                new_ngrams <- paste(new_ngrams, 
                                    tokens[(i + 1):(length(tokens) - width + 1 + i)], 
                                    sep = concatenator)
        }
        # paste onto previous results and continue
        all_ngrams <- c(all_ngrams, new_ngrams)
    }
    
    all_ngrams
}



## formerly in ngrams.R

############### OLDER, NOT USED
ngramSingle <- function(text, n=2, concatenator="_", include.all=FALSE, ...) {
    # tokenize once
    tokens <- tokenize(text, simplify=TRUE, ...)
    
    # start with lower ngrams, or just the specified size if include.all = FALSE
    start <- ifelse(include.all, 1, n)
    
    # set max size of ngram at max length of tokens
    end <- ifelse(length(tokens) < n, length(tokens), n)
    
    all_ngrams <- c()
    # outer loop for all ngrams down to 1
    for (width in start:end) {
        new_ngrams <- tokens[1:(length(tokens) - width + 1)]
        # inner loop for ngrams of width > 1
        if (width > 1) {
            for (i in 1:(width - 1)) 
                new_ngrams <- paste(new_ngrams, 
                                    tokens[(i + 1):(length(tokens) - width + 1 + i)], 
                                    sep = concatenator)
        }
        # paste onto previous results and continue
        all_ngrams <- c(all_ngrams, new_ngrams)
    }
    
    all_ngrams
}


# > ngramSingle <- function(text, n=2, concatenator="_", include.all=FALSE, ...) {
#     +     tokens <- tokenize(text, simplify=TRUE, ...)
#     +     
#         +     new_ngrams <- tokens[1:(length(tokens) - n + 1)]
#         +     if (n==1) return(new_ngrams)
#         +     for (i in 2:n) 
#             +         new_ngrams <- paste(new_ngrams, tokens[(i):(length(tokens)-n+i)], sep = concatenator) 
#             +     
#                 +     if (include.all & n > 1)
#                     +         new_ngrams <- c(new_ngrams, ngramSingle(text, n-1, concatenator, include.all, ...))
#                     +     new_ngrams
#                     + }

# Create bigrams
# 
# @author Ken Benoit and Kohei Watanabe
# @param text character vector containing the texts from which bigrams will be
#   constructed
# @param window how many words to be counted for adjacency.  Default is 1 for
#   only immediately neighbouring words.  This is only available for bigrams,
#   not for ngrams.
# @param concatenator character for combining words, default is \code{_}
#   (underscore) character
# @param include.unigrams if \code{TRUE}, return unigrams as well
# @param ignoredFeatures a character vector of features to ignore
# @param skipGrams If \code{FALSE} (default), remove any bigram containing a
#   feature listed in \code{ignoredFeatures}, otherwise, first remove the
#   features in \code{ignoredFeatures}, and then create bigrams.  This means
#   that some "bigrams" will actually not occur as adjacent features in the
#   original text.  See examples.
# @param ... provides additional arguments passed to \link{tokenize}
# @return a character vector of bigrams
# @export
# @examples 
# bigrams("The quick brown fox jumped over the lazy dog.")
# bigrams(c("The quick brown fox", "jumped over the lazy dog."))
# bigrams(c("The quick brown fox", "jumped over the lazy dog."), window=2)
# bigrams(c("I went to tea with her majesty Queen Victoria.", "Does tea have extra caffeine?"))
# bigrams(c("I went to tea with her majesty Queen Victoria.", "Does tea have extra caffeine?"), 
#         ignoredFeatures=stopwords("english"))
# bigrams(c("I went to tea with her majesty Queen Victoria.", "Does tea have extra caffeine?"), 
#         ignoredFeatures=stopwords("english"), skipGrams=TRUE)
bigrams <- function(text, window = 1, concatenator="_", include.unigrams=FALSE, 
                    ignoredFeatures=NULL, skipGrams=FALSE, ...) {
    
    catm("note: bigrams() is being phased out, and replaced by tokenize(x, ngrams=2)\n")
    
    removeIgnoredFeatures <- function(bigramCharVector, ignoredFeatures) {
        ignoredfeatIndex <- 
            grep(paste0("\\b", paste(ignoredFeatures, collapse="\\b|\\b"), "\\b"), 
                 gsub("_", " ", bigramCharVector))
        if (length(ignoredfeatIndex) > 0) 
            bigramCharVector <- bigramCharVector[-ignoredfeatIndex]
        bigramCharVector
    }
    
    tokenizedList <- tokenize(text, ...)
    
    if (!is.null(ignoredFeatures) & skipGrams==TRUE)
        tokenizedList <- lapply(tokenize(text), removeIgnoredFeatures, ignoredFeatures)
    
    bigramSingle <- function(tokens, window, concatenator, include.unigrams) {
        bigrams <- c()  # initialize bigrams vector
        for (w in (1:window)) {
            m1 <- c(rep('', w), tokens)
            m2 <- c(tokens, rep('', w))
            b <- paste(m1, m2, sep=concatenator)
            l <- length(b)
            bigrams <- c(bigrams, b[(w+1):(l-w)])
        }
        if (include.unigrams) bigrams <- c(tokens, bigrams)
        bigrams
    }
    
    result <- lapply(tokenizedList, bigramSingle, window, concatenator, include.unigrams)
    
    # remove features if supplied and if skipGrams==FALSE, in other words
    # remove all bigrams that contain an ignored feature
    if (!is.null(ignoredFeatures) & skipGrams==FALSE) {
        if (!is.character(ignoredFeatures)) 
            stop("ignoredFeatures must be a character vector")
        result <- lapply(result, removeIgnoredFeatures, ignoredFeatures)
    }
    
    result
}
#bigrams("aa bb cc dd ee ff", 5)

