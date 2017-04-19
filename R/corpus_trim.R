#' remove sentences based on their token lengths or a pattern match
#' 
#' Removes sentences from a corpus or a character vector shorter than a 
#' specified length.
#' @param x \link{corpus} or character object whose sentences will be selected.
#' @param what units of triming, sentences or paragraphs
#' @param min_ntok,max_ntok minimum and maximum lengths in word tokens 
#'   (excluding punctuation)
#' @param exclude_pattern a \pkg{stringi} regular expression whose match (at the
#'   sentence level) will be used to exclude sentences
#' @param return_tokens if \code{TRUE}, return tokens object of sentences after
#'   trimming, otherwise return the input object type with the trimmed sentences
#'   removed.
#' @return a \link{corpus} or character vector equal in length to the input, or
#'   a tokenized set of sentences if .  If the input was a corpus, then the all
#'   docvars and metadata are preserved.  For documents whose sentences have
#'   been removed entirely, a null string (\code{""}) will be returned.
#' @export
#' @keywords internal experimental
#' @examples
#' txt <- c("PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
#'          "PAGE 2. Very short! Shorter.",
#'          "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
#' mycorp <- corpus(txt, docvars = data.frame(serial = 1:3))
#' texts(mycorp)
#' 
#' # exclude sentences shorter than 3 tokens
#' texts(corpus_trim(mycorp, min_ntok = 3))
#' # exclude sentences that start with "PAGE <digit(s)>"
#' texts(corpus_trim(mycorp, exclude_pattern = "^PAGE \\d+"))
#' 
#' # on a character
#' char_trim(txt, min_ntok = 3)
corpus_trim <- function(x, what = c("sentences", "paragraphs"),
                        min_ntok = 1, max_ntok = NULL, exclude_pattern = NULL) {
    UseMethod("corpus_trim")
}

#' @noRd
#' @export
corpus_trim.corpus <- function(x, what = c("sentences", "paragraphs"),
                               min_ntok = 1, max_ntok = NULL, exclude_pattern = NULL) {
    
    what <- match.arg(what)
    if(is.null(max_ntok)) max_ntok <- 1e10 
    
    # segment corpus
    temp <- corpus_reshape(x, to = "sentences")
    
    # exclude based on lengths
    length <- ntoken(temp, remove_punct = TRUE)
    temp <- corpus_subset(temp, length >= min_ntok & length <= max_ntok)
    
    # exclude based on regular expression match
    if (!is.null(exclude_pattern)) {
        temp <- corpus_subset(temp, !stri_detect_regex(texts(temp), exclude_pattern))
    }
    result <- corpus_reshape(temp, to = "document")
    
    return(result)
}


#' @rdname corpus_trim
#' @export
#' @examples
#' char_trim(txt, "sentences", min_ntok = 3)
#' char_trim(txt, "sentences", exclude_pattern = "sentence\\.")
char_trim <- function(x, what = c("sentences", "paragraphs"), 
                      min_ntok = 1, max_ntok = NULL, exclude_pattern = NULL) {
    UseMethod("char_trim")
}

#' @noRd
#' @export
char_trim.character <- function(x, what = c("sentences", "paragraphs"), 
                                min_ntok = 1, max_ntok = NULL, exclude_pattern = NULL) {
    texts(corpus_trim(corpus(x), what, min_ntok, max_ntok, exclude_pattern))
}
