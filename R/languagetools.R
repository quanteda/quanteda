
#' Quantitative Analysis of Textual Data
#'
#' Quantitative Analysis of Textual Data
#'
#' @name quanteda
#' @docType package

countSyllables <- function(sourceText){
  #load the RData file
  data(syllableCounts)
  #clean the string
  string <- gsub("[[:punct:][:digit:]]", "", sourceText)
  string <- gsub("\n", "", string)
  string <- toupper(string)
  words <- unlist(strsplit(string, " "))
  # lookup the syllables in the words found in the dictionary
  # uses vectorization and named vector indexing - not looping!
  n.syllables <- counts[words]
  # name the syllable count vector with the words
  names(n.syllables) <- words
  # count the syllables in each word?
  vowel.count.lookup <- sapply(words, function(l) sum((attr(gregexpr("[AEIOUY]*", l)[[1]], "match.length"))!=0))
  # replace the un-looked-up words with vowel formula words
  n.syllables[is.na(n.syllables)] <- 
    vowel.count.lookup[is.na(n.syllables)]
  return(sum(n.syllables))
}


#' Returns a table of the occurrences of differen parts of speech in a sentence

#' This function takes a sentence and tags each word with it's part of speech using 
#' openNLP's POS tagger, then returns a table of the parts of speech
#' 
#' @param text Text to be tagged
#' @export
#' @examples
#' determine.pos(sentence)
determine.pos <- function(sentence) {
  # clean sentence of punctuation and numbers
  require(openNLP)
  sentence <- gsub("[[:punct:][:digit:]]", "", sentence)
  print(sentence)
  # tage sentence parts of speech
  tagged.sentence <- tagPOS(sentence)
  # tokenize
  tagged.sentence.pos.char.vector <- scan(what="char", text=tagged.sentence, quiet=TRUE)
  # create a list of splits on the / character that precedes POS tags
  strsplit(tagged.sentence.char.vector, "/")
  tagged.sentence.pos.parsedlist <- strsplit(tagged.sentence.pos.char.vector, "/")
  # put the second element of the list into a (factor) vector
  tagged.sentence.pos.factor.vector <-
    factor(sapply(tagged.sentence.pos.parsedlist, function(x) x[2]))
  # name the vector with the word
  names(tagged.sentence.pos.factor.vector) <-
    sapply(tagged.sentence.pos.parsedlist, function(x) x[1])
  # convert to table of POS and return as list
  return(as.list(table(tagged.sentence.pos.factor.vector)))
} 


#' split a text into words and return a table of words and their counts 

#' This function takes a text (in the form of a character vectors),
#' performs some cleanup, and splits the text on whitespace, returning
#' a dataframe of words and their frequncies
#' 
#' @param text Text to be tokenized
#' @export
#' @examples
#' tokenize(text)
tokenize <- function(text, textname='count'){
  # returns a dataframe of word counts, word is 1st column
  #
  ## clean up stuff in the text
  clean.txt <- gsub("[[:punct:][:digit:]]", "", text)
  # for French, make "l'" into "l"
  text <- gsub("l'", "l ", text)
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
  names(wf.list) <- c("feature", textname)
  return(wf.list)
}

