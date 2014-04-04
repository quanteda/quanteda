## Ken's notes
# should use tokeniser instead of just splitting on space delimiters
# help function needs arguments explained, and returns
# we should generalize this to ngrams(text, n=2, window=1, unordered=FALSE)
# -- meaning we need a new, more general version to supercede this
# -- Note the suggested defaults above - default window should be 1, not 2
# -- we probably never really want to make unordered=TRUE since these are not
#    naturally occurring pairs, just like "savings bank" is not the same as 
#    "bank savings"
# -- possibly define a version for a corpus
# -- right now it treats a text vector as a single text, which is fine, 
#    but we will want to note this in the help file.  To apply to a vector
#    of texts we will need to apply() it or define a corpus method.

#' Create bigrams
#' 
#' @param tokens
#' @author kohei Watanabe
#' @return a character of bigrams vector
#' @export
#' @examples
#' bigrams("aa bb cc dd ee ff")
#' bigrams(")
bigrams <- function(text, window = 2, unordered = FALSE){
    ## should use our tokenizer instead - KB
    t <- unlist(strsplit(text, ' '))

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

    # Sort words in bigrams alphabetically - but not really the bigrams since it reorders them!
    if (unordered) {
        bigrams <- unlist(lapply(bigrams, function(x) paste(sort(unlist(strsplit(x, '_'))), collapse = '_' )))
    }
    #bigramText <- paste(bigrams, collapse = ' ')
    return(bigrams)
}
#bigrams("aa bb cc dd ee ff", 5)

