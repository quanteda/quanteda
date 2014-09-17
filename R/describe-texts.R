#' print a summary of texts 

#' Prints to the console a desription of the texts, including 
#' number of types, tokens, and sentences
#' 
#' @param txts The texts to be described
#' @param verbose Default is TRUE. Set to false to suppress output messages
#' @export
#' @examples
#' describeTexts(c("testing this text", "and this one"))
#' describeTexts(uk2010immig)
describeTexts <- function(txts, verbose=TRUE) {
    # need to implement subsetting here too
    if (is.null(names(txts))) 
        names(txts) <- paste("text", 1:length(txts), sep="")
    tokenizedTexts <- tokenize(txts)
    ntokens <- sapply(tokenizedTexts, length)
    temp <- lapply(tokenizedTexts, unique)
    ntypes <- sapply(temp, length)
    # because we still don't have a generic sentence segmenter
    # broken
    nsents  <- sapply(txts, function(s) length(gregexpr("[.!?]", s)[[1]]))
    results <- data.frame(Text=names(txts),
                          Types=ntypes,
                          Tokens=ntokens,
                          Sentences=nsents,
                          row.names=NULL)
    if (verbose) print(results)
    return(invisible(results))
}