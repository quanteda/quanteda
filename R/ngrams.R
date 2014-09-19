#' Create bigrams
#' 
#' @author Ken Benoit and Kohei Watanabe
#' @param text character vector containing the texts from which bigrams will be constructed
#' @param window how many words to be counted for adjacency.  Default is 1 for only immediately 
#' neighbouring words.  This is only available for bigrams, not for \link{ngram}.
#' @param concatenator character for combining words, default is \code{_} (underscore) character
#' @param include.unigrams if \code{TRUE}, return unigrams as well
#' @param ... additional arguments passed to \link{tokenize}
#' @return a character vector of bigrams
#' @export
#' @examples 
#' bigrams("The quick brown fox jumped over the lazy dog.")
#' bigrams(c("The quick brown fox", "jumped over the lazy dog."))
#' bigrams(c("The quick brown fox", "jumped over the lazy dog."), window=2)
bigrams <- function(text, window = 1, concatenator="_", include.unigrams=FALSE, ...) {
    tokenizedList <- tokenize(text, ...)
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
    lapply(tokenizedList, bigramSingle, window, concatenator, include.unigrams)
}
#bigrams("aa bb cc dd ee ff", 5)


#' Create ngrams
#' 
#' Create a set of ngrams (words in sequence) from text(s) in a character vector
#' @author Ken Benoit, Kohei Watanabe, Paul Nulty
#' @return a list of character vectors of ngrams, one list element per text
#' @param text character vector containing the texts from which ngrams will be extracted
#' @param n the number of tokens to concatenate. Default is 2 for bigrams.
#' @param window how many words to be counted for adjacency.  Default is 1 for only immediately 
#' neighbouring words.
#' @param concatenator character for combining words, default is \code{_} (underscore) character
#' @param include.all if TRUE, add n-1...1 grams to the returned list
#' @param ... additional arguments passed to \link{tokenize}
#' @export
#' @examples 
#' ngrams("The quick brown fox jumped over the lazy dog.", n=2)
#' identical(ngrams("The quick brown fox jumped over the lazy dog.", n=2),
#'           bigrams("The quick brown fox jumped over the lazy dog.", n=2))
#' ngrams("The quick brown fox jumped over the lazy dog.", n=3)
#' ngrams("The quick brown fox jumped over the lazy dog.", n=3, concatenator="~")
#' ngrams("The quick brown fox jumped over the lazy dog.", n=3, include.all=TRUE)
ngrams <- function(text, n=2, concatenator="_", include.all=FALSE, ...) {
    lapply(text, ngramSingle, n, concatenator, include.all, ...)
}

ngramSingle <- function(text, n=2, concatenator="_", include.all=FALSE, ...) {
    t <- unlist(tokenize(text, ...))
    len <- length(t)
    ngram.result <- c()  # initialize ngrams vector
    tl <- list()   # initialize tl vector
    min <- n
    if (include.all==TRUE) min <- 1 # set minimum to unigram if include.all
    
    for (i in (1:n)) {
        tl[[i]] <- t[(i):(len-(n-i))]
    }
    # ngrams <- do.call('paste', c(t(tl)[1:n], sep = concatenator))
    # paste the elements together
    ngram.result <- do.call('paste', c(tl, sep = concatenator))
    # build up list with successive calls if include.all is TRUE
    if (include.all==TRUE) {
        for (i in (n-1):1) {
            ngram.result <- c(ngramSingle(text, i, concatenator, ...), ngram.result)
        }
    }
    return(ngram.result)
}

