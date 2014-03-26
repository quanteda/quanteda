#' print a summary of texts 

#' Prints to the console a desription of the texts, including 
#' number of types, tokens, and sentences
#' 
#' @param texts The texts to be described
#' @export
#' @examples
#' texts <- c("testing this text", "and this one")
#' describeTexts(texts)
describeTexts <- function(texts, output=TRUE) {
  # need to implement subsetting here too
  if (is.null(names(texts))) 
    names(texts) <- paste("text", 1:length(texts), sep="")
  cleanTexts <- lapply(texts,clean)
  tokenizedTexts <- sapply(cleanTexts, tokenize)
  ntokens <- sapply(tokenizedTexts,length)
  ntypes <- length(unique)
  temp <- sapply(tokenizedTexts, unique)
  ntypes <- sapply(temp, length)
  nsents  <- sapply(texts, function(s) length(gregexpr("[.!?]", s)[[1]]))
  if (output) {
    cat("Summary of texts:\n")
    print(data.frame(Texts=names(texts),
                     Types=ntypes,
                     Tokens=ntokens,
                     Sentences=nsents,
                     row.names=NULL))
  }
}