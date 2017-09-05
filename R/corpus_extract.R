#' extract and store values in corpus
#' 
#' Extract characters from douments with regular expression pattern and store
#' them in \link{corpus} in corpus.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param field a name of a \link{docvars} field to store extracted values
#' @param keep_pattern keep matched patters in documents if \code{TRUE}
#' @param ... not used
#' @return \code{corpus_extract} returns a corpus of texts, with extracted 
#'   values in \link{docvars}.

#' @examples
#' corp <- 
#' corpus(c("##INTRO This is the introduction.",
#'          "##DOC1 This is the first document.",
#'          "##DOC2 This is the second document.",
#'          "##DOC3 This is the third document."))
#' corp <- corpus_extract(corp, '##*', valuetype = "glob", field = 'tag')
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
#' corp2_seg <- corpus_extract(corp2_seg, '##[A-Z0-9]+', valuetype = "regex", field = 'tag')
#' head(corp2_seg)
#' head(docvars(corp2_seg))
#' 
#' @export
corpus_extract <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), 
                           field, keep_pattern = FALSE, ...) {
    UseMethod("corpus_extract")
}

#' @noRd
#' @rdname corpus_extract
#' @export    
corpus_extract.corpus <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), 
                                  field, keep_pattern = FALSE, ...) {
    
    valuetype <- match.arg(valuetype)
    
    match <- char_extract(texts(x), pattern, valuetype)
    docvars(x, field) <- stri_trim_both(match)
    if (!keep_pattern)
        texts(x) <- stri_replace_first_fixed(texts(x), match, '')
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
char_extract <- function(x, pattern, valuetype, ...) {
    UseMethod("char_extract")
}
        
#' @noRd
#' @export
char_extract.character <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), ...) {
    
    valuetype <- match.arg(valuetype)
    
    if (valuetype == "glob") {
        # treat as fixed if no glob character is detected
        if (!any(stri_detect_charclass(pattern, c("[*?]")))) {
            valuetype <- "fixed"
        } else {
            regex <- escape_regex(pattern)
            regex <- stri_replace_all_fixed(regex, '*', '(\\S*)')
            regex <- stri_replace_all_fixed(regex, '?', '(\\S)')
            pattern <- stri_c(regex, collapse = '|')
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "regex") {
        match <- stri_extract_first_regex(x, pattern)
    } else {
        match <- stri_extract_first_fixed(x, pattern)
    }
    return(match)
}

