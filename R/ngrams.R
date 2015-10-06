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
    
    if (length(x) < min(n)) return(NULL)
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
#' @param k for skip-grams only, \code{k} is the number of skips used to 
#'   construct the $n$-gram. Skip-grams for a given skip distance $k$ allow a 
#'   total of $k$ or less skips to construct the $n$-gram.  Thus a 
#'   "4-skip-n-gram" produces results that include 4 skips, 3 skips, 2 skips, 1 
#'   skip, and 0 skips (where 0 skips are typical n-grams formed from adjacent
#'   words).  See Guthrie et al (2006).

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
#'   D., B. Allison, W. Liu, and L. Guthrie. 2006. "A Closer Look at Skip-Gram
#'   Modelling."}
#' @importFrom utils combn
#' @examples 
#' tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), 
#'                    removePunct = TRUE, simplify = TRUE)
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



