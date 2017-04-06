#' remove sentences based on their token lengths or a pattern match
#' 
#' Removes sentences from a corpus or a character vector shorter than a 
#' specified length.
#' @param x \link{corpus} or character object whose sentences will be selected.
#' @param min_length,max_length minimum and maximum lengths in word tokens 
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
#' texts(corpus_trimsentences(mycorp, min_length = 3))
#' # exclude sentences that start with "PAGE <digit(s)>"
#' texts(corpus_trimsentences(mycorp, exclude_pattern = "^PAGE \\d+"))
#' 
#' # on a character
#' char_trimsentences(txt, min_length = 3)
corpus_trimsentences <- function(x, min_length = 1, max_length = 10000, 
                                 exclude_pattern = NULL,
                                 return_tokens = FALSE) {
    UseMethod("corpus_trimsentences")
}

#' @noRd
#' @export
corpus_trimsentences.corpus <- function(x, min_length = 1, max_length = 10000, 
                                        exclude_pattern = NULL,
                                        return_tokens = FALSE) {
    ntok <- NULL
    temp_sentences <- corpus_reshape(x, to = "sentences")
    
    # exclude based on lengths
    docvars(temp_sentences, "ntok") <-  ntoken(temp_sentences, remove_punct = TRUE)
    temp_sentences <- corpus_subset(temp_sentences, ntok >= min_length & ntok <= max_length)
    docvars(temp_sentences, "ntok") <- NULL
    
    # exclude based on regular expression match
    if (!is.null(exclude_pattern)) {
        temp_sentences <- corpus_subset(temp_sentences,
                                        !stri_detect_regex(texts(temp_sentences),
                                                           exclude_pattern))
    }
    corpus_reshape(temp_sentences, to = "document")
}


#' @rdname corpus_trimsentences
#' @export
#' @examples
#' char_trimsentences(txt, min_length = 3)
#' char_trimsentences(txt, exclude_pattern = "sentence\\.")
char_trimsentences <- function(x, min_length = 1, max_length = 10000, exclude_pattern = NULL) {
    UseMethod("char_trimsentences")
}

#' @noRd
#' @export
char_trimsentences.character <- function(x, min_length = 1, max_length = 10000, exclude_pattern = NULL) {
    texts(corpus_trimsentences(corpus(x), min_length, max_length, exclude_pattern))
}

#' #' @rdname corpus_trimsentences
#' #' @export
#' tokens_trimsentences <- function(x, min_length = 1, max_length = 10000, 
#'                                  exclude_pattern = NULL) {
#'     UseMethod("tokens_trimsentences")
#' }
#'     
#' #' @noRd
#' #' @export
#' tokens_trimsentences.tokens <- function(x, min_length = 1, max_length = 10000, 
#'                                         exclude_pattern = NULL) {
#'     if (attr(x, "what" != "sentence"))
#'         stop("tokens_trimsentences only works if what = \"sentence\"")
#'     
#'     # trim based on length
#'     x <- x[]
#' }
#' 
#' lapply(x, function(y) { 
#'     ret <- y[which(ntoken(y, remove_punct = TRUE) > 2)]
#' })

