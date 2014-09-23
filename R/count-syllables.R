#' Returns a count of the number of syllables in the input
#'
#' This function takes a text and returns a count of the number of syllables it contains.
#' For British English words, the syllable count is exact and looked up from the CMU
#' pronunciation dictionary. For any word not in the dictionary the syllable count
#' is estimated by counting vowel clusters.
#' 
#' @param sourceText Character vector of texts whose syllables will be counted 
# @param verbose If True, print out the count. Default false.
#' 
#' @return numeric Named vector of counts of the number of syllables for each element of sourceText.
#' When a word is not available in the lookup table, its syllables are estimated by counting the number
#' of (English) vowels in the word.
#' @export
#' @details This only works for English.
#' @examples
#' countSyllables("This is an example sentence.")
#' myTexts <- c("Text one.", "Superduper text number two.", "One more for the road.")
#' names(myTexts) <- paste("myText", 1:3, sep="")
#' countSyllables(myTexts)
countSyllables <- function(sourceText) { #}, verbose=FALSE) {

    data(syllableCounts)               # load the syllable counts data
    string <- clean(sourceText)        # clean the input text(s)
    words <- lapply(string, tokenize)  # tokenize the input text(s)
    
    # match syllable counts to words in the list
    nSyllables <- lapply(words, function(x) syllableCounts[unlist(x)])
    
    # look up vowel counts for those not in the lst
    for (i in 1:length(nSyllables)) {
        naIndex <- which(is.na(nSyllables[[i]]))
        if (length(naIndex)==0) next
        # get the missing words as names
        names(nSyllables[[i]])[naIndex] <- words[[i]][naIndex]
        # replace NA counts with vowel counts    
        nSyllables[[i]][naIndex] <- vowelCount(words[[i]][naIndex])
    }
    return(sapply(nSyllables, sum))
}


## return a vector of vowel counts from a vector of texts
vowelCount <- function(textVector) {
    vowel.index.list <- 
        lapply(gregexpr("[AEIOUYaeiouy]*", textVector), attr, "match.length")
    sapply(vowel.index.list, sum)
}
