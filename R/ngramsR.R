#' Create ngrams (R method)
#' 
#' Create a set of ngrams (words in sequence) from tokenized text(s)
#' @author Ken Benoit
#' @return a tokenizedTexts object consisting a list of character vectors of 
#'   ngrams, one list element per text, or a character vector if called on a 
#'   simple character vector
#' @param x a tokenizedText object or a character vector of tokens
#' @param n integer vector specifying the number of elements to be concatenated 
#'   in each ngram
#' @param skip integer vector specifying the adjacency skip size for tokens 
#'   forming the ngrams, default is 0 for only immediately neighbouring words. 
#'   For \code{skipgrams}, \code{skip} can be a vector of integers, as the
#'   "classic" approach to forming skip-grams is to set skip = $k$ where $k$ is
#'   the distance for which $k$ or fewer skips are used to construct the
#'   $n$-gram.  Thus a "4-skip-n-gram" defined as \code{skip = 0:4} produces
#'   results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0 skips (where
#'   0 skips are typical n-grams formed from adjacent words).  See Guthrie et al
#'   (2006).
#' @param concatenator character for combining words, default is \code{_} 
#'   (underscore) character
#' @export
#' @examples
#' ngramsR(LETTERS, n = 2, skip = 2)
#' ngramsR(LETTERS, n = 3, skip = 2)
#' ngramsR(LETTERS, n = 3, skip = 3)
#' 
#' tokens <- tokenize("the quick brown fox jumped over the lazy dog.", 
#'                    removePunct = TRUE, simplify = TRUE)
#' ngramsR(tokens, n = 1:3)
#' ngramsR(tokens, n = c(2,4), concatenator = " ")
#' ngramsR(tokens, n = c(2,4), skip = 1, concatenator = " ")
#' 
#' ## form ngrams
#' ngrams(LETTERS[1:6], n = 3) # "A_B_C" "B_C_D" "C_D_E" "D_E_F"
#' ngramsR(LETTERS[1:6], n = 3)   # "A_B_C" "B_C_D" "C_D_E" "D_E_F"
#' 
#' tokens <- tokenize("the quick brown fox jumped over the lazy dog.",
#'                    removePunct = TRUE, simplify = TRUE)
#' ngramsR(tokens, n = c(2,4), concatenator = " ")
#' ngrams(tokens, n = c(2,4), concatenator = " ")
#' 
#' ngrams(LETTERS[1:6], n = 3, skip = 0) #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
#' ngramsR(LETTERS[1:6], n = 3, skip = 0)   #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
#' ngrams(LETTERS[1:6], n = 3, skip = 1) #"A_C_E" "B_D_F"
#' ngramsR(LETTERS[1:6], n = 3, skip = 1)   #"A_C_E" "B_D_F"
#' ngrams(LETTERS[1:6], n = 3, skip = 2) # None
#' ngramsR(LETTERS[1:6], n = 3, skip = 2)   # None
#' ngrams(LETTERS[1:6], n = 2, skip = 2) # [1] "A_D" "B_E" "C_F"
#' ngramsR(LETTERS[1:6], n = 2, skip = 2)   # [1] "A_D" "B_E" "C_F"
#' 
#' # skipgrams
ngramsR <- function(x, ...) {
    UseMethod("ngramsR")
}

#' @rdname ngramsR
#' @importFrom stats complete.cases
#' @export
ngramsR.character <- function(x, n = 2, skip = 0, concatenator = "_", ...) {
    window <- skip + 1
    if (any(stringi::stri_detect_fixed(x, " ")) & concatenator != " ")
        stop("whitespace detected: please tokenize() before using ngrams()")
    
    if (length(x) < min(n)) return(NULL)
    if (identical(n, 1)) {
        if (!identical(window, 1))
            warning("skip argument ignored for n = 1")
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
#' @rdname ngramsR
#' @export
ngramsR.tokenizedTexts <- function(x, n = 2, skip = 0, concatenator = "_", ...) {
    ngramsResult <- parallel::mclapply(x, ngramsR.character, n, skip, concatenator, ...)
    class(ngramsResult) <- c("tokenizedTexts", class(ngramsResult))
    attributes(ngramsResult) <- attributes(x)
    ngramsResult
}


#' @rdname ngramsR
#' @details Normally, \code{\link{ngramsR}} will be called through 
#'   \code{\link{tokenize}}, but these functions are also exported in case a 
#'   user wants to perform lower-level ngram construction on tokenized texts.
#'   
#'   \code{\link{skipgramsR}} is a wrapper to \code{\link{ngramsR}} that simply 
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
#' skipgramsR(tokens, n = 2, skip = 1, concatenator = " ") 
#' skipgramsR(tokens, n = 2, skip = 2, concatenator = " ") 
#' skipgramsR(tokens, n = 3, skip = 2, concatenator = " ")   
skipgramsR <- function(x, ...) UseMethod("skipgramsR")

#' @rdname ngramsR
#' @export
skipgramsR.character <- function(x, n = 2, skip = 1, concatenator = "_", ...) {
    if (n < skip) {
        warning("n cannot be less than skip for skipgrams, returning NULL")
        return(NULL)
    }    
    ngrams2(x, n, window = skip + 1, concatenator, skipgrams = TRUE)
}

#' @rdname ngramsR
#' @export
skipgramsR.tokenizedTexts <- function(x, n = 2, skip = 1, concatenator = "_", ...)
    parallel::mclapply(x, skipgramsR.character, n, skip, concatenator, ...)


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

#' ## form skipgrams
#' (sc <- skipgrams_c(LETTERS[1:6], n = 2, skip = 0:1))
#' (s <- skipgrams(LETTERS[1:6], n = 2, skip = 1))
#' !length(setdiff(sc, s))
#' (sc <- skipgrams_c(LETTERS[1:6], n = 2, skip = 0:2))
#' (s <- skipgrams(LETTERS[1:6], n = 2, skip = 2))
#' !length(setdiff(sc, s))
#'
#' tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), 
#'                     removePunct = TRUE, simplify = TRUE)
#' (sc <- skipgrams(tokens, n = 2, skip = 1, concatenator = " "))
#' (s <- skipgrams_c(tokens, n = 2, skip = 0:1, concatenator = " "))
#' !length(setdiff(sc, s))
#' 
#' (s <- skipgrams(tokens, n = 2, skip = 2, concatenator = " "))
#' (sc <- skipgrams_c(tokens, n = 2, skip = 0:2, concatenator = " "))
#' !length(setdiff(sc, s))
#' 
#' (s <- skipgrams(tokens, n = 3, skip = 2, concatenator = " "))
#' (sc <- skipgrams_c(tokens, n = 3, skip = 0:2, concatenator = " "))
#' !length(setdiff(sc, s))

##
## UNCOMMENT AND EXECUTE FOR SPEED COMPARISONS
##
# require(microbenchmark)
# mycheck <- function(values)
#     all(sapply(values[-1], function(x) identical(sort(values[[1]]), sort(x))))
# 
# toksN3 <- tokenize(toLower(inaugTexts[1:5]), removePunct = TRUE)
# microbenchmark(unit = "relative", times = 50,
#                ngrams_R = ngrams(toks, n = 3),
#                ngrams_Cpp = lapply(toks, ngrams_c, n = 3))
# 
# microbenchmark(unit = "relative", times = 50,
#                skipgrams_R = skipgrams(toks, n = 3, skip = 2),
#                skipgrams_Cpp = lapply(toks, skipgrams_c, n = 3, skip = 2))

