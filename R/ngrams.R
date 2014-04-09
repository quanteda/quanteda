
#' Create bigrams
#' 
#' @param tokens
#' @author kohei Watanabe
#' @return a character of bigrams vector
#' @export
#' @examples 
#' data(iebudgets)
#' wfm <- dfm(iebudgets, group="party", bigram=TRUE)
bigrams <- function(text, window = 2){
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

