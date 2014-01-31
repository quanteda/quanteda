describeTexts <-
function(texts, output=TRUE) {
  # need to implement subsetting here too
  string <- gsub("[[:punct:][:digit:]]", "", texts)
  string <- gsub("\n", "", string)
  string <- string[string!=""]
  string <- tolower(string)
  tokenized.string <- lapply(string, function(s) strsplit(s, c(" ", ".", "?", "!")))
  ntokens <- sapply(tokenized.string, function(s) sapply(s, length))
  ntypes  <- sapply(tokenized.string, function(s) sapply(s, function(s2) length(unique(s2))))
  nsents  <- sapply(texts, function(s) length(gregexpr("[.!?]", s)[[1]]))
  if (output) {
    cat("Summary of texts:\n", substitute(texts), sep="")
    print(data.frame(Texts=names(texts),
                     Types=ntypes,
                     Tokens=ntokens,
                     Sentences=nsents,
                     row.names=NULL))
  }
  return(invisible(list(ntokens=ntokens, ntypes=ntypes, nsents=nsents)))
}
