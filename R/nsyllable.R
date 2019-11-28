#' @rdname data-internal
#' @details
#' `data_int_syllables` provides an English-language syllables dictionary;
#' it is an integer vector whose element names correspond to English words.
"data_int_syllables"

#' Count syllables in a text
#' 
#' @description Returns a count of the number of syllables in texts. For English
#'   words, the syllable count is exact and looked up from the CMU pronunciation
#'   dictionary, from the default syllable dictionary `data_int_syllables`. 
#'   For any word not in the dictionary, the syllable count is estimated by 
#'   counting vowel clusters.
#'   
#'   `data_int_syllables` is a quanteda-supplied data object consisting of a 
#'   named numeric vector of syllable counts for the words used as names.  This 
#'   is the default object used to count English syllables.  This object that 
#'   can be accessed directly, but we strongly encourage you to access it only 
#'   through the `nsyllable()` wrapper function.
#'   
#' @param x character vector or `tokens` object  whose 
#'   syllables will be counted.  This will count all syllables in a character 
#'   vector without regard to separating tokens, so it is recommended that x be 
#'   individual terms.
#' @param syllable_dictionary optional named integer vector of syllable counts where 
#'   the names are lower case tokens.  When set to `NULL` (default), then 
#'   the function will use the quanteda data object `data_int_syllables`, an 
#'   English pronunciation dictionary from CMU.
#' @param use.names logical; if `TRUE`, assign the tokens as the names of 
#' the syllable count vector
#'   
#' @return If `x` is a character vector, a named numeric vector of the 
#'   counts of the syllables in each element.  If `x` is a [tokens]
#'   object, return a list of syllable counts where each list element corresponds
#'   to the tokens in a document.
#' @note All tokens are automatically converted to lowercase to perform the
#'   matching with the syllable dictionary, so there is no need to perform this
#'   step prior to calling `nsyllable()`.
#'
#'   `nsyllable()` only works reliably for English, as the only syllable count
#'   dictionary we could find is the freely available CMU pronunciation
#'   dictionary at `http://www.speech.cs.cmu.edu/cgi-bin/cmudict`.  If you
#'   have a dictionary for another language, please email the package
#'   maintainer as we would love to include it.
#' @name nsyllable
#' @export
#' @examples
#' # character
#' nsyllable(c("cat", "syllable", "supercalifragilisticexpialidocious", 
#'             "Brexit", "Administration"), use.names = TRUE)
#' 
#' # tokens
#' txt <- c(doc1 = "This is an example sentence.",
#'          doc2 = "Another of two sample sentences.")
#' nsyllable(tokens(txt, remove_punct = TRUE))
#' # punctuation is not counted
#' nsyllable(tokens(txt), use.names = TRUE)
nsyllable <- function(x, syllable_dictionary = quanteda::data_int_syllables, 
                      use.names = FALSE) {
    UseMethod("nsyllable")
}

#' @export
nsyllable.default <- function(x, syllable_dictionary = quanteda::data_int_syllables, 
                              use.names = FALSE) {
    stop(friendly_class_undefined_message(class(x), "nsyllable"))
}

#' @rdname nsyllable
#' @noRd
#' @export
nsyllable.character <- function(x, syllable_dictionary = quanteda::data_int_syllables, 
                                use.names = FALSE) { 
    # look up syllables
    result <- syllable_dictionary[char_tolower(x, keep_acronyms = FALSE)]
    # keep or discard names
    if (use.names) {
        names(result) <- x
    } else {
        result <- unname(result)
    }
    # count vowels if the word did not match the syllable dictionary
    result[is.na(result)] <- 
        stringi::stri_count_regex(x[is.na(result)], "[aeiouy]+", case_insensitive = TRUE)
    # so we don't words with no vowels as having syllables
    result[which(result == 0)] <- NA
    result
}


#' @rdname nsyllable
#' @noRd
#' @examples 
#' \dontshow{
#' txt <- c(one = "super freakily yes",
#'          two = "merrily all go aerodynamic")
#' toks <- tokenize(txt)
#' toksh <- tokens(txt)
#' nsyllable(toks)
#' nsyllable(toksh)
#' }
#' @export
nsyllable.tokens <- function(x, syllable_dictionary = quanteda::data_int_syllables, 
                             use.names = FALSE) { 
    types <- types(x)
    if (attr(x, 'padding')) {
        vocab_sylls <- nsyllable(c("", types), use.names = use.names)
        lapply(unclass(x), function(y) vocab_sylls[y + 1]) 
    } else {
        vocab_sylls <- nsyllable(types, use.names = use.names)
        lapply(unclass(x), function(y) vocab_sylls[y]) 
    }
}
