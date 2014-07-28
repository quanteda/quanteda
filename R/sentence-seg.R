#' split a text into sentences

#' This function takes a text and splits it into sentences.
#' 
#' @param text Text to be segmented
#' @param pat The regular expression for recognizing end of sentence delimiters.
#' @param abbreviations A list of abbreviations'.' and therefore should not be used to segment text
#' @param stripempty Remove empty "sentences", TRUE by default.  Should only be set to false if 
#' for some reason you wanted to preserve the original text with all of its spaces etc.
#' @export
#' @examples
#' test <- "This is a sentence! Several sentences. It's designed by a Dr. to test whether this function works. Or not? Or not."
#' sentenceSeg(test)
sentenceSeg <- function(text, pat="[\\.\\?\\!][\\n* ]|\\n\\n*", abbreviations = NULL,
                        stripempty = TRUE) {
    stops <- unlist(strsplit(text, split=pat, perl=TRUE) )
    if (is.null(abbreviations)) {abbreviations <- c('Mr', 'Mrs', 'Ms', 'Dr','Jr','Prof')}
    i <- 1
    sentences <- c()
    
    while (i < length(stops)+1) {
        exception <- FALSE
        # it is an exception if the last word is an abbreviation OR if the 
        # next token is not uppercase
        lastword <- tail(unlist(strsplit(stops[[i]], " ")),1)
        
        # don't want to look to the next sentence if this is the last sentence
        if (i==length(stops)) {
            sentences <- c(sentences, stops[[i]])
            break;
        }
        
        # check if the last word before the . is an abbreviation
        if (!length(lastword)==0) {
            if (lastword %in% abbreviations) {exception <- TRUE}
        }
        
        # check if the first letter of word after the . is upper case
        nextChar <- substr(stops[[i+1]],1,1)
        if(nextChar != toupper(nextChar)) {
            exception <- TRUE
        }
        
        # if we think this . is not a sentence boundary, paste the current
        # phrase and the phrase after the . together.
        if (exception) {
            sentences <- c(sentences, paste(stops[[i]],stops[[i+1]],'. '))
            i <- i + 2
        } else {
            sentences <- c(sentences, stops[[i]])
            i <- i + 1
        }
    }
    sentences <- sapply(sentences, gsub, pattern="\\n", replacement="")
    # remove spaces only and empty "sentences"
    if (stripempty) 
        sentences <- sentences[!(sentences==" " | sentences=="")]
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