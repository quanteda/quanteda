

#' count syllables in a text
#' 
#' @description This function takes a text and returns a count of the number of 
#'   syllables it contains. For British English words, the syllable count is
#'   exact and looked up from the CMU pronunciation dictionary, from the default
#'   syllable dictionary \code{englishSyllables}. For any word not in the 
#'   dictionary the syllable count is estimated by counting vowel clusters.
#'   
#'   \code{englishSyllables} is a quanteda-supplied data object consisting of a 
#'   named numeric vector of syllable counts for the words used as names.  This
#'   is the default object used to count English syllables.  This object that
#'   can be accessed directly, but we strongly encourage you to access it only
#'   through the \code{syllables()} wrapper function.
#'   
#' @param x character vector or list of character vectors whose syllables will 
#'   be counted
#' @param syllableDict optional named integer vector of syllable counts where
#'   the names are lower case tokens.  When set to \code{NULL} (default), then
#'   the function will use the quanteda data object \code{englishSyllables}, an
#'   English pronunciation dictionary from CMU.
#' @param ... additional arguments passed to clean
#'   
#' @return numeric Named vector or list of counts of the number of syllables for
#'   each element of x. When a word is not available in the lookup table, its 
#'   syllables are estimated by counting the number of (English) vowels in the 
#'   word.
#' @name syllables
#' @export
#' @examples
#' syllables("This is an example sentence.")
#' syllables(tokenize("This is an example sentence.", simplify=TRUE))
#' myTexts <- c(text1 = "Text one.", 
#'              text2 = "Superduper text number two.", 
#'              text3 = "One more for the road.")
#' syllables(myTexts)
#' syllables("supercalifragilisticexpialidocious")
syllables <- function(x, ...) {
    UseMethod("syllables")
}

#' @rdname syllables
#' @export
syllables.character <- function(x, syllableDict = NULL, ...) { 
    englishSyllables <- NULL
    if (is.null(syllableDict)) {
        data(englishSyllables, envir = environment())
        syllableDict <- englishSyllables
    } else {
        if (!is.integer(syllableDict))
            stop("user-supplied syllableDict must be named integer vector.")
    }
    words <- tokenize(x, ...)
    
    # match syllable counts to words in the list
    nSyllables <- lapply(words, function(x) syllableDict[x])
    
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
