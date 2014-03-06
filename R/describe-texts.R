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
  string <- gsub("[[:punct:][:digit:]]", "", texts)
  string <- gsub("\n", "", string)
  string <- string[string!=""]
  string <- tolower(string)
  tokenized.string <- lapply(string, function(s) strsplit(s, c(" ", ".", "?", "!")))
  ntokens <- sapply(tokenized.string, function(s) sapply(s, length))
  ntypes  <- sapply(tokenized.string, function(s) sapply(s, function(s2) length(unique(s2))))
  nsents  <- sapply(texts, function(s) length(gregexpr("[.!?]", s)[[1]]))
  if (output) {
    cat("Summary of texts:\n")
    print(data.frame(Texts=names(texts),
                     Types=ntypes,
                     Tokens=ntokens,
                     Sentences=nsents,
                     row.names=NULL))
  }
  return(invisible(list(ntokens=ntokens, ntypes=ntypes, nsents=nsents)))
}