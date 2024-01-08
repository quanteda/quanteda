#' Remove sentences based on their token lengths or a pattern match
#'
#' Removes sentences from a corpus or a character vector shorter than a
#' specified length.
#' @param x [corpus] or character object whose sentences will be selected.
#' @param what units of trimming, `"sentences"` or `"paragraphs"`, or
#'   `"documents"`
#' @param min_ntoken,max_ntoken minimum and maximum lengths in word tokens
#'   (excluding punctuation).  Note that these are approximate numbers of tokens
#'   based on checking for word boundaries, rather than on-the-fly full
#'   tokenisation.
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
#' corp
#'
#' # exclude sentences shorter than 3 tokens
#' corpus_trim(corp, min_ntoken = 3)
#' # exclude sentences that start with "PAGE <digit(s)>"
#' corpus_trim(corp, exclude_pattern = "^PAGE \\d+")
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
    min_ntoken <- check_integer(min_ntoken, min = 0)
    max_ntoken <- check_integer(max_ntoken, allow_null = TRUE)
    exclude_pattern <- check_character(exclude_pattern, allow_null = TRUE)

    # segment corpus
    temp <- corpus_reshape(x, to = what)
    
    if (length(temp) == 0)
        return(x)
    
    # exclude based on lengths
    len <- stringi::stri_count_boundaries(temp, type = "word", skip_word_none = TRUE)
    if (is.null(max_ntoken))
        max_ntoken <- max(len)
    result <- corpus_subset(temp, len >= min_ntoken & len <= max_ntoken)

    # exclude based on regular expression match
    if (!is.null(exclude_pattern)) {
        is_pattern <- stri_detect_regex(result, exclude_pattern)
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
    as.character(corpus_trim(corpus(x), what, min_ntoken, max_ntoken, exclude_pattern))
}
