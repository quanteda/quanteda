#' Create bigrams
#' 
#' @param tokens
#' @author Kohei Watanabe and Ken Benoit
#' @param text character vector containing the texts from which ngrams will be extracted
#' @param window how many words to be counted for adjacency.  Default is 1 for only immediately 
#' neighbouring words.
#' @return a character of bigrams vector
#' @export
#' @examples 
#' data(iebudgets)
#' bigrams("The quick brown fox jumped over the lazy dog.")
#' bigrams("The quick brown fox jumped over the lazy dog.", window=2)
bigrams <- function(text, window = 1) {
    ## should use our tokenizer instead - KB
    #t <- unlist(strsplit(text, ' '))
    t <- tokenize(text)
    # initialize bigrams vector
    bigrams <- c()
    # w <- 1  - not nececssary - for() initializes the w using the range of in
    for (w in (1:window)) {
        m1 <- c(rep('', w), t)
        m2 <- c(t, rep('', w))
        b <- paste(m1, m2, sep='_')
        #print(b)
        l <- length(b)
        bigrams <- c(bigrams, b[(w+1):(l-w)])
    }

    return(bigrams)
}
#bigrams("aa bb cc dd ee ff", 5)

#' Create ngrams
#' 
#' Create a set of ngrams (words in sequence) from a text.
#' 
#' @author Kohei Watanabe, Ken Benoit, Paul Nulty
#' @return a character of bigrams vector
#' @param text character vector containing the texts from which ngrams will be extracted
#' @param n the number of tokens to concatenate. Default is 2 for bigrams.
#' @param window how many words to be counted for adjacency.  Default is 1 for only immediately 
#' neighbouring words.
#' @param concatenator character for combining words, default is \code{_} (underscore) character
#' @export
#' @examples 
#' data(iebudgets)
#' ngrams("The quick brown fox jumped over the lazy dog.", n=2)
#' ngrams("The quick brown fox jumped over the lazy dog.", n=3)
ngrams <- function(text, n=2, window=1, concatenator="_") {
    ## GENERALIZE to NGRAMS
    t <- tokenize(text)
    # initialize ngrams vector
    ngrams <- c()
    for (w in (1:window)) {
        m1 <- c(rep('', w), t)
        m2 <- c(t, rep('', w))
        b <- paste(m1, m2, sep=concatenator)
        #print(b)
        # trim the ends
        l <- length(b)
        ngrams <- c(ngrams, b[(w+1+(n-2)):(l-w-(n-2))])
    }
    return(ngrams)
}
