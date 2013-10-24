# These functions perform linguistic analysis on strings of text, as opposed
# to those in corpustools which operate on corpus objects, feature-value matrices
# and files

#' Returns a count of the number of syllables in the input

#' This function takes a text and returns a count of the number of syllables it contains.
#' For British English words, the syllable count is exact and looked up from the CMU
#' pronunciation dictionary. For any word not in the dictionary the syllable count
#' is estimated by counting vowel clusters.
#' 
#' @param text Text to be counted
#' @export
#' @examples
#' tokenize("This is an example sentence.")
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
#' THE Penn Treebank Part of Speech tags:
#' CC Coordinating conjunction
#' CD Cardinal number
#' DT Determiner
#' EX Existential there
#' FW Foreign word
#' IN Preposition or subordinating conjunction
#' JJ Adjective
#' JJR Adjective, comparative
#' JJS Adjective, superlative
#' LS List item marker
#' MD Modal
#' NN Noun, singular or mass
#' NNS Noun, plural
#' NNP Proper noun, singular
#' NNPS Proper noun, plural
#' PDT Predeterminer
#' POS Possessive ending
#' PRP Personal pronoun
#' PRP$ Possessive pronoun
#' RB Adverb
#' RBR Adverb, comparative
#' RBS Adverb, superlative
#' RP Particle
#' SYM Symbol
#' TO to
#' UH Interjection
#' VB Verb, base form
#' VBD Verb, past tense
#' VBG Verb, gerund or present participle
#' VBN Verb, past participle
#' VBP Verb, non-3rd person singular present
#' VBZ Verb, 3rd person singular present
#' WDT Wh-determiner
#' WP Wh-pronoun
#' WP$ Possessive wh-pronoun
#' WRB Wh­adverb
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
  gc() # garbage collection - seems to prevent Heap Memory errors for Java call
  if (tagged.sentence=="") tagged.sentence<-"DeleteMe"
  # tokenize
  tagged.sentence.pos.char.vector <- scan(what="char", text=tagged.sentence, quiet=TRUE)
  # create a list of splits on the / character that precedes POS tags
  strsplit(tagged.sentence.pos.char.vector, "/")
  tagged.sentence.pos.parsedlist <- strsplit(tagged.sentence.pos.char.vector, "/")
  # put the second element of the list into a (factor) vector
  tagged.sentence.pos.factor.vector <-
    (sapply(tagged.sentence.pos.parsedlist, function(x) x[2]))
  # return as a factor vector of same length as text
  return(tagged.sentence.pos.factor.vector)
} 


#' split a text into words and return a table of words and their counts 

#' This function takes a text (in the form of a character vectors),
#' performs some cleanup, and splits the text on whitespace, returning
#' a dataframe of words and their frequncies
#' 
#' @param text Text to be tokenized
#' @export
#' @examples
#' tokenize("This is an example sentence.")
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
  if(length(tokenized.txt)>0){
    names(wf.list) <- c("feature", textname)
  }
  return(wf.list)
}



#' split a text into sentences

#' This function takes a text and splits it into sentences.
#' 
#' @param text Text to be segmented
#' @export
sentenceSeg <- function(text, pat="[\\.\\?\\!][\\n* ]|\\n\\n*", abbreviations = NULL, parag = TRUE){
  # returns a dataframe of word counts, word is 1st column
  #
  stops <- unlist(strsplit(text, split=pat, perl=TRUE) )
  if(is.null(abbreviations)) {abbreviations <- c('Mr', 'Mrs', 'Ms', 'Dr','Jr','Prof')}
  i <- 1
  sentences <- c()
  
  while(i < length(stops)+1){
    exception <- FALSE
    # it is an exception if the last word is an abbreviation OR if the 
    # next token is not uppercase
    lastword <- tail(unlist(strsplit(stops[[i]], " ")),1)
    
    # don't want to look to the next sentence if this is the last sentence
    if(i==length(stops)){
      sentences <- c(sentences, stops[[i]])
      break;
    }
    
    # check if the last word before the . is an abbreviation
    if(!length(lastword)==0){
      if (lastword %in% abbreviations){exception <- TRUE}
    }
    
    # check if the first letter of word after the . is upper case
    nextChar <- substr(stops[[i+1]],1,1)
    if(nextChar != toupper(nextChar)){
      exception <- TRUE
    }
    
    # if we think this . is not a sentence boundary, paste the current
    # phrase and the phrase after the . together.
    if(exception){
      sentences <- c(sentences, paste(stops[[i]],stops[[i+1]],'. '))
      i <- i + 2
    }else{
      sentences <- c(sentences, stops[[i]])
      i <- i + 1
    }
  }
  sentences <- lapply(sentences, gsub, pattern="\\n", replacement="")
  return(sentences)
}

# KB 2013-07-01
# sentence.delimiters can be redefined to suit the language
# takes a single text, returns a vector of sentences
# IF you want to apply rules, such as . followed by uppercase, then
# change these before the split using gsub()
sentenceSeg2 <- function(text, sentence.delimiters="[.!?]") {
  # strip out CRs and LFs, tabs
  text <- gsub("\\n\\t", "", text)     
  # split the text into sentences
  # WOULD BE NICE TO PRESERVE SENTENCE PUNCTUATION
  sentences <- unlist(strsplit(text, sentence.delimiters))
  # COULD STRIP LEADING SPACES HERE TOO
  return(sentences)
}


# remove common or 'semantically empty' words from a text.
# 
removeStopwords <- function(text, stopwords=NULL){
  
  if(stopwods == NULL) stopwords <- load('stopwords_EN')
  
  
}



