#' Remove sentences based on their token lengths or a pattern match
#'
#' Removes sentences from a corpus or a character vector shorter than a
#' specified length.
#' @param x [corpus] or character object whose sentences will be selected.
#' @param what units of trimming, `"sentences"` or `"paragraphs"`, or
#'   `"documents"`
#' @param min_ntoken,max_ntoken minimum and maximum lengths in word tokens
#'   (excluding punctuation)
#' @param exclude_pattern a \pkg{stringi} regular expression whose match (at the
#'   sentence level) will be used to exclude sentences
#' @return a [corpus] or character vector equal in length to the input.  If
#'   the input was a corpus, then the all docvars and metadata are preserved.
#'   For documents whose sentences have been removed entirely, a null string
#'   (`""`) will be returned.
#' @export
#' @keywords corpus
#' @examples
#' txt <- c("PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
#'          "PAGE 2. Very short! Shorter.",
#'          "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
#' corp <- corpus(txt, docvars = data.frame(serial = 1:3))
#' texts(corp)
#'
#' # exclude sentences shorter than 3 tokens
#' texts(corpus_trim(corp, min_ntoken = 3))
#' # exclude sentences that start with "PAGE <digit(s)>"
#' texts(corpus_trim(corp, exclude_pattern = "^PAGE \\d+"))
#'
corpus_trim <- function(x, what = c("sentences", "paragraphs", "documents"),
                        min_ntoken = 1, max_ntoken = NULL,
                        exclude_pattern = NULL) {
    UseMethod("corpus_trim")
}

#' @noRd
#' @export
corpus_trim.corpus <- function(x, what = c("sentences", "paragraphs", "documents"),
                               min_ntoken = 1, max_ntoken = NULL,
                               exclude_pattern = NULL) {
    x <- as.corpus(x)
    what <- match.arg(what)
    if (is.null(max_ntoken)) max_ntoken <- 1e10

    # segment corpus
    temp <- corpus_reshape(x, to = what)

    # exclude based on lengths
    length <- ntoken(temp, remove_punct = TRUE)
    result <- corpus_subset(temp, length >= min_ntoken & length <= max_ntoken)

    # exclude based on regular expression match
    if (!is.null(exclude_pattern)) {
        is_pattern <- stri_detect_regex(texts(result), exclude_pattern)
        result <- corpus_subset(result, !is_pattern)
    }
    if (what != "documents" && ndoc(result) > 0)
        result <- corpus_reshape(result, to = "documents")
    return(result)
}


#' @rdname corpus_trim
#' @keywords character
#' @export
#' @examples
#' # trimming character objects
#' char_trim(txt, "sentences", min_ntoken = 3)
#' char_trim(txt, "sentences", exclude_pattern = "sentence\\.")
char_trim <- function(x, what = c("sentences", "paragraphs", "documents"),
                      min_ntoken = 1, max_ntoken = NULL, exclude_pattern = NULL) {
    UseMethod("char_trim")
}

#' @noRd
#' @export
char_trim.character <- function(x, what = c("sentences", "paragraphs", "documents"),
                                min_ntoken = 1, max_ntoken = NULL, exclude_pattern = NULL) {
    what <- match.arg(what)
    texts(corpus_trim(corpus(x), what, min_ntoken, max_ntoken, exclude_pattern))
}


#' Remove sentences based on their token lengths or a pattern match
#'
#' Removes sentences from a corpus or a character vector shorter than a
#' specified length.
#' @param x [corpus] or character object whose sentences will be selected.
#' @param min_length,max_length minimum and maximum lengths in word tokens
#'   (excluding punctuation)
#' @param exclude_pattern a \pkg{stringi} regular expression whose match (at the
#'   sentence level) will be used to exclude sentences
#' @param return_tokens if `TRUE`, return tokens object of sentences after
#'   trimming, otherwise return the input object type with the trimmed sentences
#'   removed.
#' @return a [corpus] or character vector equal in length to the input, or
#'   a tokenized set of sentences if .  If the input was a corpus, then the all
#'   docvars and metadata are preserved.  For documents whose sentences have
#'   been removed entirely, a null string (`""`) will be returned.
#' @note This function has been superseded by [corpus_trim()]; use
#'   that function instead.
#' @export
#' @keywords internal deprecated
#' @examples
#' txt <- c("PAGE 1. A single sentence.  Short sentence. Three word sentence.",
#'          "PAGE 2. Very short! Shorter.",
#'          "Very long sentence, with three parts, separated by commas.  PAGE 3.")
#' corp <- corpus(txt, docvars = data.frame(serial = 1:3))
#' texts(corp)
#'
#' # exclude sentences shorter than 3 tokens
#' texts(corpus_trimsentences(corp, min_length = 3))
#' # exclude sentences that start with "PAGE <digit(s)>"
#' texts(corpus_trimsentences(corp, exclude_pattern = "^PAGE \\d+"))
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

    temp <- corpus_reshape(x, to = "sentences")

    # exclude based on lengths
    length <- ntoken(temp, remove_punct = TRUE)
    temp <- corpus_subset(temp, length >= min_length & length <= max_length)

    # exclude based on regular expression match
    if (!is.null(exclude_pattern)) {
        is_pattern <- stri_detect_regex(texts(temp), exclude_pattern)
        temp <- corpus_subset(temp, !is_pattern)
    }
    corpus_reshape(temp, to = "document")
}


#' @rdname corpus_trimsentences
#' @export
#' @examples
#' char_trimsentences(txt, min_length = 3)
#' char_trimsentences(txt, exclude_pattern = "sentence\\.")
char_trimsentences <- function(x, min_length = 1, max_length = 10000,
                               exclude_pattern = NULL) {
    UseMethod("char_trimsentences")
}

#' @noRd
#' @export
char_trimsentences.character <- function(x, min_length = 1, max_length = 10000,
                                         exclude_pattern = NULL) {
    texts(corpus_trimsentences(corpus(x), min_length, max_length, exclude_pattern))
}
