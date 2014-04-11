
#' Create bigrams
#' 
#' @param tokens
#' @author kohei Watanabe
#' @return a character of bigrams vector
#' @export
#' @examples 
#' data(iebudgets)
#' wfm <- dfm(iebudgets, group="party", bigram=TRUE)
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
#' @param tokens
#' @author Kohei Watanabe, Ken Benoit, Paul Nulty
#' @return a character of bigrams vector
#' @export
#' @examples 
#' data(iebudgets)
#' ngrams("The quick brown fox jumped over the lazy dog.", n=2)
#' ngrams("The quick brown fox jumped over the lazy dog.", n=3)
ngrams <- function(text, n=2, window=1) {
    ## GENERALIZE to NGRAMS
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
    
    return(ngrams)
}
