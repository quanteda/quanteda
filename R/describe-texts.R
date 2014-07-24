#' print a summary of texts 

#' Prints to the console a desription of the texts, including 
#' number of types, tokens, and sentences
#' 
#' @param texts The texts to be described
#' @param verbose Default is TRUE. Set to false to suppress output messages
#' @export
#' @examples
#' texts <- c("testing this text", "and this one")
#' describeTexts(texts)
describeTexts <- function(texts, verbose=TRUE) {
    # need to implement subsetting here too
    if (is.null(names(texts))) 
        names(texts) <- paste("text", 1:length(texts), sep="")
    cleanTexts <- lapply(texts,clean)
    tokenizedTexts <- lapply(cleanTexts, tokenize)
    ntokens <- sapply(tokenizedTexts,length)
    temp <- lapply(tokenizedTexts, unique)
    ntypes <- sapply(temp, length)
    # because we still don't have a generic sentence segmenter
    nsents  <- sapply(texts, function(s) length(gregexpr("[.!?]", s)[[1]]))
    results <- data.frame(Texts=names(texts),
                          Types=ntypes,
                          Tokens=ntokens,
                          Sentences=nsents,
                          row.names=NULL)
    if (verbose) print(results)
    return(results)
}