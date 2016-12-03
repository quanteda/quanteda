#' @rdname data-internal
#' @details
#' \code{data_int_syllables} provides an English-language syllables dictionary; it is
#' an integer vector whose element names correspond to English words. Built from
#' the freely available CMU pronunciation dictionary at 
#' \code{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}.
"data_int_syllables"

#' deprecated name for nsyllable
#' 
#' Deprecated function name; use \code{\link{nsyllable}} instead.
#' @export
#' @keywords internal deprecated
syllables <- function(x, ...) {
    .Deprecated("nsyllable")
    nsyllable(x, ...)
}

#' count syllables in a text
#' 
#' @description Returns a count of the number of syllables in texts. For English
#'   words, the syllable count is exact and looked up from the CMU pronunciation
#'   dictionary, from the default syllable dictionary \code{data_int_syllables}. 
#'   For any word not in the dictionary, the syllable count is estimated by 
#'   counting vowel clusters.
#'   
#'   \code{data_int_syllables} is a quanteda-supplied data object consisting of a 
#'   named numeric vector of syllable counts for the words used as names.  This 
#'   is the default object used to count English syllables.  This object that 
#'   can be accessed directly, but we strongly encourage you to access it only 
#'   through the \code{nsyllable()} wrapper function.
#'   
#' @param x character vector or \code{tokens} object  whose 
#'   syllables will be counted
#' @param syllable_dictionary optional named integer vector of syllable counts where 
#'   the names are lower case tokens.  When set to \code{NULL} (default), then 
#'   the function will use the quanteda data object \code{data_int_syllables}, an 
#'   English pronunciation dictionary from CMU.
#' @param ... additional arguments passed to \code{\link{tokens}}
#'   
#' @return If \code{x} is a character vector, a named numeric vector of the 
#'   counts of the syllables in each text, without tokenization.  If \code{x} 
#'   consists of (a list of) tokenized texts, then return a list of syllable 
#'   counts corresponding to the tokenized texts.
#' @note All tokens are automatically converted to lowercase to perform the
#'   matching with the syllable dictionary, so there is no need to perform this
#'   step prior to calling \code{nsyllable()}.
#' @name nsyllable
#' @export
#' @examples
#' nsyllable("This is an example sentence.")
#' nsyllable(tokenize("This is an example sentence.", simplify=TRUE))
#' myTexts <- c(text1 = "Text one.", 
#'              text2 = "Superduper text number two.", 
#'              text3 = "One more for the road.")
#' nsyllable(myTexts)
#' nsyllable(tokenize(myTexts, removePunct = TRUE))
#' nsyllable("supercalifragilisticexpialidocious")
nsyllable <- function(x, syllable_dictionary = quanteda::data_int_syllables, ...) {
    UseMethod("nsyllable")
}

#' @rdname nsyllable
#' @noRd
#' @export
nsyllable.character <- function(x, syllable_dictionary = quanteda::data_int_syllables, ...) { 
    tokenizedwords <- tokenize(x, removePunct = TRUE, removeTwitter = TRUE, removeNumbers = TRUE, ...)
    sapply(nsyllable(tokenizedwords, syllable_dictionary), sum)
}


#' @rdname nsyllable
#' @noRd
#' @examples 
#' \dontshow{
#' txt <- c(one = "super freakily yes",
#'                 two = "merrily all go aerodynamic")
#' toks <- tokenize(txt)
#' toksh <- tokens(txt)
#' nsyllable(toks)
#' nsyllable(toksh)
#' }
#' @export
nsyllable.tokens <- function(x, syllable_dictionary = quanteda::data_int_syllables, ...) { 
    vocab_sylls <- nsyllable(types(x))
    sapply(unclass(x), function(y) vocab_sylls[y])
}

#' @rdname nsyllable
#' @noRd
#' @export
nsyllable.tokenizedTexts <- function(x, syllable_dictionary = quanteda::data_int_syllables, ...) { 
    
    # make tokenized list into a data table
    syllablesDT <- data.table(docIndex = rep(1:length(x), lengths(x)),
                              word = unlist(x), 
                              serial = 1:length(unlist(x)))
    
    # call the syllables data.table function
    nSyllables <- nsyllable.data.table(syllablesDT, syllable_dictionary)    
    
    # restore names
    names(nSyllables) <- names(x)    
    
    nSyllables
}


nsyllable.data.table <- function(x, syllable_dictionary = quanteda::data_int_syllables, ...) {
    word <- serial <- NULL
    
    # retrieve or validate syllable list
    data_int_syllables <- NULL
    if (is.null(syllable_dictionary)) {
        #data(data_int_syllables, envir = environment())
        #syllable_dictionary <- data_int_syllables
    } else {
        if (!is.integer(syllable_dictionary))
            stop("user-supplied syllable_dictionary must be named integer vector.")
    }
    
    # make syllable list into a data table
    syllable_dictionaryDT <- data.table(word = names(syllable_dictionary), syllables = syllable_dictionary)
    setkey(syllable_dictionaryDT, word)
    
    # lowercase the tokenizedTexts object, needed for the matching in the next step
    x[, word := toLower(word)]

    # set the key for x
    setkey(x, word)
    
    # merge words to get syllables
    # suppressWarnings so it won't complain about mixed encodings
    suppressWarnings(syllDT <- syllable_dictionaryDT[x])
    
    # look up vowel counts for those not in the syllables list
    syllDT[is.na(syllables), syllables := stringi::stri_count_regex(word, "[aeiouy]+")]
    # put back into original order
    syllDT <- syllDT[order(serial)]
    # split back to a list
    syllcount <- split(syllDT[, syllables], syllDT$docIndex)

    syllcount
}


