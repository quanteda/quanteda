#' extract and store values in corpus
#' 
#' Extract characters from douments with regular expression pattern and store
#' them in \link{corpus} in corpus.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @param pattern a regular expression pattern that match characters in
#'   documents
#' @param field a name of a \link{docvars} field to store extracted values
#' @param remove remove matched characters from documents if TRUE
#' @return \code{corpus_extract} returns a corpus of texts, with extracted 
#'   values in \link{docvars}.

#' @examples
#' txts <- c("##INTRO This is the introduction.",
#'           "##DOC1 This is the first document.",
#'           "##DOC2 This is the second document.",
#'           "##DOC3 This is the third document.")
#' corp <- corpus(txts)
#' corp <- corpus_extract(corp, '##[A-Z0-9]+', 'tag', remove = TRUE)
#' head(corp)
#' head(docvars(corp))
#' 
#' @author Kenneth Benoit
#' @export
corpus_extract <- function(x, pattern, field, remove = TRUE, ...) {
    UseMethod("corpus_extract")
}

#' @noRd
#' @rdname corpus_extract
#' @export    
corpus_extract.corpus <- function(x, pattern, field, remove = TRUE, ...) {
    
    docvars(x, field) <- char_extract(texts(x), pattern)
    if (remove) {
        texts(x) <- stri_replace_first_regex(texts(x), pattern, '')
    }
    return(x)
}

#' @rdname corpus_extract
#' @export
#' @examples
#' ## segmenting a character object
#' 
#' txts <- c("##INTRO This is the introduction.",
#'           "##DOC1 This is the first document.",
#'           "##DOC2 This is the second document.",
#'           "##DOC3 This is the third document.")
#' char_extract(txts, '##[A-Z0-9]+')
#' @keywords character
#' @return \code{char_extract} returns a character vector of matched characters
char_extract <- function(x, pattern, ...) {
    UseMethod("char_extract")
}
        
#' @noRd
#' @export
char_extract.character <- function(x, pattern, ...) {
        
    if (!all(is.character(x)))
        stop("x must be of character type")
    match <- stri_extract_first_regex(x, pattern)
    match <- stri_trim_both(match)
    return(match)
}

