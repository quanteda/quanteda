#' Returns a table of the occurrences of different parts of speech in a sentence

#' This function takes a sentence and tags each word with it's part of speech using 
#' openNLP's POS tagger, then returns a table of the parts of speech
#'
#' http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
#' 
#' @param sentence Sentence to be tagged
#' @export
#' @examples
#' \dontrun{
#' tagPos("This is an example sentence with nouns and verbs for tagging.")
#' }
tagPos <- function(sentence) {
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