#' Create ngrams
#' 
#' Create a set of ngrams (words in sequence) from tokenized text(s)
#' @author Ken Benoit
#' @return a tokenizedTexts object consisting a list of character vectors of
#'   ngrams, one list element per text, or a character vector if called on a
#'   simple character vector
#' @param x a tokenizedText object or a character vector of tokens
#' @param n integer vector specifying the number of elements to be concatenated
#'   in each ngram
#' @param window integer vector specifying the adjacency width for tokens 
#'   forming the ngrams, default is 1 for only immediately neighbouring words
#' @param concatenator character for combining words, default is \code{_} 
#'   (underscore) character
#' @export
#' @examples
#' ngrams(LETTERS, n = 2, window = 2)
#' ngrams(LETTERS, n = 3, window = 2)
#' ngrams(LETTERS, n = 3, window = 3)
#' 
#' tokens <- tokenize("the quick brown fox jumped over the lazy dog.", 
#'                    removePunct = TRUE, simplify = TRUE)
#' ngrams(tokens, n = 1:3)
#' ngrams(tokens, n = c(2,4), window = 1:2, concatenator = " ")
#'
#' # skipgrams
ngrams <- function(x, ...) {
    UseMethod("ngrams")
}

#' @rdname ngrams
#' @importFrom stats complete.cases
#' @export
ngrams.character <- function(x, n = 2, window = 1, concatenator = "_", ...) {
    if (any(stringi::stri_detect_fixed(x, " ")) & concatenator != " ")
        stop("whitespace detected: please tokenize() before using ngrams()")
    
    if (length(x) < max(n)) return(NULL)
    if (identical(n, 1)) {
        if (!identical(window, 1))
            warning("window argument ignored for n = 1")
        return(x)
    }
    
    ngrams <- c()
    for (w in window) {
        winIndex <- findWindowSequence(max(n), w)
        wordtable <- data.table(w1 = c(rep(NA, max(n)-1), x))
        for (i in 2:(length(winIndex)))
            wordtable[, paste0("w", i) := wrapVector(wordtable$w1, winIndex[i]-1, w)]

        for (nsize in n) {
            nonMissing <- stats::complete.cases(wordtable[, 1:nsize, with = FALSE])
            ngrams <- c(ngrams, 
                        apply(wordtable[nonMissing, 1:nsize, with = FALSE], 
                              1, paste, collapse = concatenator))
        }
    }    
    ngrams
}

findWindowSequence <- function(n, w) {
    seq(1, (n * w) - (w-1), by = w)
}

wrapVector <- function(x, n, window = 1) {
    if (n == length(x)) 
        return(x)
    while (n > length(x)) 
        n <- n - 20
    result <- c(x[(n+1):length(x)], x[1:n])
    if (window > 1)
        result[(length(result) - n + 1) : length(result)] <- NA
    result
}

#' @param ... additional arguments passed to \code{\link[parallel]{mclapply}}
#'   which applies \code{ngram.character()} to the \code{tokenizedTexts} list object
#' @rdname ngrams
#' @export
ngrams.tokenizedTexts <- function(x, n = 2, window = 1, concatenator = "_", ...) {
    ngramsResult <- parallel::mclapply(x, ngrams.character, n, window, concatenator, ...)
    class(ngramsResult) <- c("tokenizedTexts", class(ngramsResult))
    ngramsResult
}


#' @rdname ngrams
#' @param k for skip-grams only, \code{k} is the
#' @details Normally, \code{\link{ngrams}} will be called through 
#'   \code{\link{tokenize}}, but these functions are also exported in case a 
#'   user wants to perform lower-level ngram construction on tokenized texts.
#'   
#'   \code{\link{skipgrams}} is a wrapper to \code{\link{ngrams}} that simply 
#'   passes through a \code{window} value of \code{1:(k+1)}, conforming to the 
#'   definition of skip-grams found in Guthrie et al (2006): A $k$ skip-gram is 
#'   an ngram which is a superset of all ngrams and each $(k-i)$ skipgram until 
#'   $(k-i)==0$ (which includes 0 skip-grams).
#' @export
#' @references
#'   \href{http://homepages.inf.ed.ac.uk/ballison/pdf/lrec_skipgrams.pdf}{Guthrie,
#'   D, B Allison, W Liu, and L Guthrie. 2006. "A Closer Look at Skip-Gram
#'   Modelling."}
#' @importFrom utils combn
#' @examples 
#' tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), 
#'                    removePunct = TRUE, simplify = TRUE<)
#' skipgrams(tokens, n = 2, k = 2, concatenator = " ")   
#' skipgrams(tokens, n = 3, k = 2, concatenator = " ")   
skipgrams <- function(x, ...) UseMethod("skipgrams")

#' @rdname ngrams
#' @export
skipgrams.character <- function(x, n = 2, k = 1, concatenator = "_", ...) {
    if (n < k) {
        warning("n cannot be less than k for skipgrams, returning NULL")
        return(NULL)
    }    
    ngrams2(x, n, window = k+1, concatenator, skipgrams = TRUE)
}
    #ngrams.character(x, n, window = 1:(k+1), concatenator)

#' @rdname ngrams
#' @export
skipgrams.tokenizedTexts <- function(x, n = 2, k = 1, concatenator = "_", ...)
    parallel::mclapply(x, skipgrams.character, n, k, concatenator, ...)
    #ngrams.tokenizedTexts(x, n, window = 1:(k+1), concatenator, ...)

## to make this match Guthrie et al (2006), needs to implement R version of
## http://stackoverflow.com/questions/31847682/how-to-compute-skipgrams-in-python

# # n = 2, window = 1
# combn(c(1, 2), 2, simplify = FALSE)
# ngramIndex(2, 1)
# # n = 2, window = 2
# combn(c(1, 3), 2, simplify = FALSE)
# ngramIndex(2, 2)
# # n = 2, window = 2, skipgrams
# combn(c(1, 2, 3), 2, simplify = FALSE)
# ngramIndex(2, 2, skipgrams = TRUE)
# 
# # n = 3, window = 1
# combn(c(1, 2, 3), 3, simplify = FALSE)
# ngramIndex(3, 1)
# # n = 3, window = 2
# combn(c(1, 3, 5), 3, simplify = FALSE)
# ngramIndex(3, 2)
# # n = 3, window = 2, skipgrams
# combn(c(1, 2, 3, 4, 5), 3, simplify = FALSE)
# ngramIndex(3, 2, skipgrams = TRUE)

## n    window  start  stop  by  skipgrams
## 2      1       1      2    1      F
## 2      2       1      3    2      F
## 3      1       1      3    1      F
## 3      2       1      5    2      F
## 2      2       1      3    1      T
## 3      2       1      5    1      T


ngrams2 <- function(x, n = 2, window = 1, concatenator = "_", skipgrams = FALSE, ...) {
    ngr.index <- ngramIndex(n, window, skipgrams)
    # get rid of those indexed outside of the valid range
    ngr.index <- ngr.index[sapply(ngr.index, function(i) max(i) <= length(x))]
    # x <- c(x, rep(NA, max(unlist(ngr.index))-1))
    result <- sapply(ngr.index, function(indexes) {
        totsize <- max(unlist(indexes))
        j <- indexes[1] + 1:length(x) - 1
        ngr <- x[j]
        indexes <- indexes[-1]
        while (length(indexes)) {
            j <- indexes[1] + 1:length(x) - 1
            ngr <- paste(ngr, x[j], sep = concatenator)
            indexes <- indexes[-1]
        }
        ngr[1 : (length(x) - totsize + 1)]
#         (indexMat <- matrix(1:length(x), ncol = length(i), nrow = length(x)) + 
#                      matrix(i, nrow = length(x), ncol = length(i), byrow = TRUE) - 1)
#         indexMat[indexMat > length(x)] <- NA
#         indexMat <- indexMat[complete.cases(indexMat), ]
#         apply(indexMat, 1, function(i) paste(x[i], collapse = concatenator))
    })
    unique(as.character(unlist(result)))
}

# x <- LETTERS[1:5]; n <- 2; window <- 2; concatenator = "_"
# tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), removePunct = TRUE, simplify = TRUE)
# ngrams2(tokens, 2, 2, skipgrams = TRUE)
# ngrams2(tokens, 3, 2, skipgrams = TRUE)
 
ngramIndex <- function(n, window = 1, skipgrams = FALSE) {
#     if (n <= window)
#         stop("n must be greater than ", 
#              ifelse(skipgrams, "or equal to k", "window"))
    if (!skipgrams)
        (sequence <- seq(along.with = 1:n, by = window))
    else 
        (sequence <- seq(1, n + window - 1, by = 1))
    utils::combn(sequence, n, simplify = FALSE)
}

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
    
    cat("note: bigrams() is being phased out, and replaced by tokenize(x, ngrams=2)\n")
    
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



