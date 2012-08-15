tokenize <-
function(input.text, input.text.name="count") {
  # returns a dataframe of word counts, word is 1st column
  #
  ## clean up stuff in the text
  clean.txt <- gsub("[[:punct:][:digit:]]", "", input.text)
  # for French, make "l'" into "l"
  input.text <- gsub("l'", "l ", input.text)
  # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
  clean.txt <- gsub("ß", "ss", clean.txt)
  # make all words lowercase
  clean.txt <- tolower(clean.txt)
  # tokenize
  tokenized.txt <- scan(what="char", text=clean.txt, quiet=TRUE)
  # flush out "empty" strings caused by removal of punctuation and numbers
  tokenized.txt <- tokenized.txt[tokenized.txt!=""]
  ## tabulate word counts
  ## and return as a data frame with variables "word" and given name
  wf.list <- as.data.frame(table(tokenized.txt))
  names(wf.list) <- c("feature", input.text.name)
  return(wf.list)
}
