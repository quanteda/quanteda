

#' count syllables in a text
#' 
#' @description Returns a count of the number of syllables in texts. For English
#'   words, the syllable count is exact and looked up from the CMU pronunciation
#'   dictionary, from the default syllable dictionary \code{englishSyllables}. 
#'   For any word not in the dictionary, the syllable count is estimated by 
#'   counting vowel clusters.
#'   
#'   \code{englishSyllables} is a quanteda-supplied data object consisting of a 
#'   named numeric vector of syllable counts for the words used as names.  This 
#'   is the default object used to count English syllables.  This object that 
#'   can be accessed directly, but we strongly encourage you to access it only 
#'   through the \code{syllables()} wrapper function.
#'   
#' @param x character vector or \code{tokenizedText-class} object  whose 
#'   syllables will be counted
#' @param syllableDict optional named integer vector of syllable counts where 
#'   the names are lower case tokens.  When set to \code{NULL} (default), then 
#'   the function will use the quanteda data object \code{englishSyllables}, an 
#'   English pronunciation dictionary from CMU.
#' @param ... additional arguments passed to tokenize
#'   
#' @return If \code{x} is a character vector, a named numeric vector of the 
#'   counts of the syllables in each text, without tokenization.  If \code{x} 
#'   consists of (a list of) tokenized texts, then return a list of syllable 
#'   counts corresponding to the tokenized texts.
#' @note All tokens are automatically converted to lowercase to perform the
#'   matching with the syllable dictionary, so there is no need to perform this
#'   step prior to calling \code{syllables()}.
#' @name syllables
#' @export
#' @examples
#' syllables("This is an example sentence.")
#' syllables(tokenize("This is an example sentence.", simplify=TRUE))
#' myTexts <- c(text1 = "Text one.", 
#'              text2 = "Superduper text number two.", 
#'              text3 = "One more for the road.")
#' syllables(myTexts)
#' syllables(tokenize(myTexts, removePunct = TRUE))
#' syllables("supercalifragilisticexpialidocious")
syllables <- function(x, ...) {
    UseMethod("syllables")
}

#' @rdname syllables
#' @export
syllables.character <- function(x, syllableDict = quanteda::englishSyllables, ...) { 
    tokenizedwords <- tokenize(x, removePunct = TRUE, removeTwitter = TRUE, removeNumbers = TRUE, ...)
    sapply(syllables(tokenizedwords, syllableDict), sum)
}


#' @rdname syllables
#' @export
syllables.tokenizedTexts <- function(x, syllableDict = quanteda::englishSyllables, ...) { 

    # make tokenized list into a data table
    syllablesDT <- data.table(docIndex = rep(1:length(x), lengths(x)),
                              word = unlist(x), 
                              serial = 1:length(unlist(x)))
    
    # call the syllables data.table function
    nSyllables <- syllables.data.table(syllablesDT, syllableDict)    

    # restore names
    names(nSyllables) <- names(x)    
    
    nSyllables
}


syllables.data.table <- function(x, syllableDict = quanteda::englishSyllables, ...) {
    word <- serial <- NULL
    
    # retrieve or validate syllable list
    englishSyllables <- NULL
    if (is.null(syllableDict)) {
        #data(englishSyllables, envir = environment())
        #syllableDict <- englishSyllables
    } else {
        if (!is.integer(syllableDict))
            stop("user-supplied syllableDict must be named integer vector.")
    }
    
    # make syllable list into a data table
    syllableDictDT <- data.table(word = names(syllableDict), syllables = syllableDict)
    setkey(syllableDictDT, word)
    
    # lowercase the tokenizedTexts object, needed for the matching in the next step
    x[, word := toLower(word)]

    # set the key for x
    setkey(x, word)
    
    # merge words to get syllables
    # suppressWarnings so it won't complain about mixed encodings
    suppressWarnings(syllDT <- syllableDictDT[x])
    
    # look up vowel counts for those not in the syllables list
    syllDT[is.na(syllables), syllables := stringi::stri_count_regex(word, "[aeiouy]+")]
    # put back into original order
    syllDT <- syllDT[order(serial)]
    # split back to a list
    syllcount <- split(syllDT[, syllables], syllDT$docIndex)

    syllcount
}

