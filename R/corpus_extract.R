#' extract and store values in corpus
#' 
#' Extract characters from douments with regular expression pattern and store
#' them in \link{corpus} in corpus.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @param pattern a regular expression pattern that match characters in
#'   documents
#' @param field a name of a \link{docvars} field to store extracted values
#' @param trim trim whitespaces around the matched values if \code{TRUE}
#' @param remove remove matched characters from documents if \code{TRUE}
#' @return \code{corpus_extract} returns a corpus of texts, with extracted 
#'   values in \link{docvars}.

#' @examples
#' corp <- 
#' corpus(c("##INTRO This is the introduction.",
#'          "##DOC1 This is the first document.",
#'          "##DOC2 This is the second document.",
#'          "##DOC3 This is the third document."))
#' corp <- corpus_extract(corp, '##[A-Z0-9]+', 'tag', remove = TRUE)
#' head(corp)
#' head(docvars(corp))
#' 
#' corp2 <- 
#' corpus(c("##INTRO This is the introduction.
#'           ##DOC1 This is the first document.  Second sentence in Doc 1.
#'           ##DOC3 Third document starts here.  End of third document.",
#'          "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' corp2_seg <- corpus_segment(corp2, "other", delimiter = "##", 
#'                             valuetype = "fixed", position = "before")
#' corp2_seg <- corpus_extract(corp2_seg, '##[A-Z0-9]+', 'tag', remove = TRUE)
#' head(corp2_seg)
#' head(docvars(corp2_seg))
#' 
#' @author Kenneth Benoit
#' @export
corpus_extract <- function(x, pattern, field, trim = TRUE, remove = TRUE, ...) {
    UseMethod("corpus_extract")
}

#' @noRd
#' @rdname corpus_extract
#' @export    
corpus_extract.corpus <- function(x, pattern, field, trim = TRUE, remove = TRUE, ...) {
    
    docvars(x, field) <- char_extract(texts(x), pattern, trim)
    if (remove)
        texts(x) <- stri_replace_first_regex(texts(x), pattern, '')
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
char_extract <- function(x, pattern, trim = TRUE, ...) {
    UseMethod("char_extract")
}
        
#' @noRd
#' @export
char_extract.character <- function(x, pattern, trim = TRUE, ...) {
        
    if (!all(is.character(x)))
        stop("x must be of character type")
    match <- stri_extract_first_regex(x, pattern)
    if (trim)
        match <- stri_trim_both(match)
    return(match)
}

