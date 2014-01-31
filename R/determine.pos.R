determine.pos <-
function(sentence) {
  # clean sentence of punctuation and numbers
  require(openNLP)
  sentence <- gsub("[[:punct:][:digit:]]", "", sentence)
  print(sentence)
  # tage sentence parts of speech
  tagged.sentence <- tagPOS(sentence)
  gc() # garbage collection - seems to prevent Heap Memory errors for Java call
  if (tagged.sentence=="") tagged.sentence<-"DeleteMe"
  # tokenize
  tagged.sentence.pos.char.vector <- tokenize(tagged.sentence)
  # create a list of splits on the / character that precedes POS tags
  strsplit(tagged.sentence.pos.char.vector, "/")
  tagged.sentence.pos.parsedlist <- strsplit(tagged.sentence.pos.char.vector, "/")
  # put the second element of the list into a (factor) vector
  tagged.sentence.pos.factor.vector <-
    (sapply(tagged.sentence.pos.parsedlist, function(x) x[2]))
  # return as a factor vector of same length as text
  return(tagged.sentence.pos.factor.vector)
}
