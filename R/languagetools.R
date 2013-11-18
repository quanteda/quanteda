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



#' Perform basic cleanup on a string

#' Simple cleanup for strings, removing punctuation, converting to lowercase
#' and optionally replacing some language-specific characters
#' 
#' @param s String to be cleaned
#' @export
#' @examples
#' clean(s)
clean <- function(s, langNorm=FALSE){
  s <- gsub("[:punct:]", "", s)
  s <- tolower(s)
  # optionally do some language specific normalisation
  if(langNorm){
    # for French, make "l'" into "l"
    s <- gsub("l'", "l ", s)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    s <- gsub("ß", "ss", s)
  }
  return(s)
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
tokenize <- function(text, textname='count', stem=FALSE) {
  # returns a dataframe of word counts, word is 1st column
  #
  ## clean up stuff in the text
  clean.txt <- gsub("[[:punct:][:digit:]]", "", text)
  # for French, make "l'" into "l"
  clean.txt <- gsub("l'", "l ", clean.txt)
  # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
  clean.txt <- gsub("ß", "ss", clean.txt)
  # make all words lowercase
  clean.txt <- tolower(clean.txt)
  # tokenize
  tokenized.txt <- scan(what="char", text=clean.txt, quiet=TRUE)
  # flush out "empty" strings caused by removal of punctuation and numbers
  tokenized.txt <- tokenized.txt[tokenized.txt!=""]
  # stem the words if this flag is set to TRUE
  if (stem==TRUE) {
      require(SnowballC)
      tokenized.txt <- wordStem(tokenized.txt)
  }
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

likelihood.test = function(x) {
    nrows = dim(x)[1]                      # no. of rows in contingency table
    ncols = dim(x)[2]                      # no. of cols in contingency table
    chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
    table = chi.out[[6]]                   # get the OFs
    ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
    sum = 0                                # storage for the test statistic
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            sum = sum + table[i,j]*log(ratios[i,j])
        }
    }
    sum = 2 * sum                          # the likelihood ratio chi square
    df = chi.out[[2]]                      # degrees of freedom
    p = 1 - pchisq(sum,df)                 # p-value
    out = c(sum, df, p, chi.out[[1]])      # the output vector
    names(out) = c("LRchi2","df","p-value","Pearschi2")
    return(as.list(out))                           # done!
}

bigrams <- function(text=NULL, file=NULL, top=NA, distance=2, method="lr") {
    ## returns the bigrams, frequency, and score as a list
    ##
    if (is.null(text) & is.null(file)) stop("Must specify either text or file.")
    clean.txt <- gsub("[[:punct:][:digit:]]", "", text)
    # for French, make "l'" into "l"
    clean.txt <- gsub("\xa7", "", clean.txt)
    # for French, make "l'" into "l"
    clean.txt <- gsub("l'", "l ", clean.txt)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    clean.txt <- gsub("ß", "ss", clean.txt)
    # make all words lowercase
    clean.txt <- tolower(clean.txt)
    # tokenize
    tokenized.txt <- scan(what="char", text=clean.txt, quiet=TRUE)
    # flush out "empty" strings caused by removal of punctuation and numbers
    t <- tokenized.txt[tokenized.txt!=""]
    t <- tolower(t)
    bigrams <- paste(t[1:(length(t)-1)], t[2:length(t)])
    bigrams <- tolower(bigrams)
    bigrams.tokenized <- as.data.frame(table(bigrams), stringsAsFactors=FALSE)
    bigrams.tokenized <- bigrams.tokenized[order(bigrams.tokenized$bigrams), ]
    bigrams.tokenized$w1 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 1)
    bigrams.tokenized$w2 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 2)
    bigrams.tokenized$test <- NA
    require(entropy)
    options(warn=-1)
    if (method=="lr") {
        for (i in 1:nrow(bigrams.tokenized)) {
            bigrams.tokenized$test[i] <-
                likelihood.test(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]))$LRchi2
        }
    } else if (method=="chi2") {
        for (i in 1:nrow(bigrams.tokenized)) {
            bigrams.tokenized$test[i] <-
                chisq.test(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]), correct=FALSE)$statistic
        }
    } else if (method=="mi") {
        for (i in 1:nrow(bigrams.tokenized)) {
            bigrams.tokenized$test[i] <-
                entropy(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i])) + entropy(table(bigrams.tokenized$w2==bigrams.tokenized$w2[i])) -
                    entropy(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]))
        }
    } else stop("method must be from: lr, chi2, or mi")
    options(warn=0)
    bigrams.tokenized <- bigrams.tokenized[order(bigrams.tokenized$test, decreasing=TRUE),]
    if (is.na(top)) top <- nrow(bigrams.tokenized)
    returnval <- bigrams.tokenized[1:top, c("bigrams", "Freq", "test")]
    # rename the statistic as the test
    names(returnval)[3] <- method
    # returns this as a (named) list
    return(as.list(returnval))
}


