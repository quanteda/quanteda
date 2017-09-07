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
#' corp <- corpus_extracttags(corp, '##*', valuetype = "glob")
#' head(corp)
#' head(docvars(corp))
#' 
#' corp2 <- 
#' corpus(c("##INTRO This is the introduction.
#'           ##DOC1 This is the first document.  Second sentence in Doc 1.
#'           ##DOC3 Third document starts here.  End of third document.",
#'          "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' corp2_seg <- corpus_extracttags(corp2, '##[A-Z0-9]+', valuetype = "regex")
#' head(corp2_seg)
#' head(docvars(corp2_seg))
#' 
#' @export
corpus_extracttags <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), 
                               position = c("after", "before"), use_docvars = TRUE, ...) {
    UseMethod("corpus_extracttags")
}

#' @noRd
#' @rdname corpus_extracttags
#' @export    
corpus_extracttags.corpus <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), 
                                      position = c("after", "before"), use_docvars = TRUE, ...) {
    
    valuetype <- match.arg(valuetype)
    temp <- corpus_segment(x, pattern, valuetype, remove_pattern = FALSE, position, use_docvars, ...) 
    match <- char_extracttags(texts(temp), pattern, valuetype, position)
    texts(temp) <- stri_trim_both(stri_replace_first_fixed(texts(x), match, ''))
    docvars(temp, 'tag') <- stri_trim_both(match)
    return(temp)
}

#' @rdname corpus_extracttags
#' @export
#' @examples
#' ## segmenting a character object
#' 
#' txts <- c("##INTRO This is the introduction.",
#'           "##DOC1 This is the first document.",
#'           "##DOC2 This is the second document.",
#'           "##DOC3 This is the third document.")
#' char_extracttags(txts, '##[A-Z0-9]+')
#' @keywords character
#' @return \code{char_extracttags} returns a character vector of matched characters
char_extracttags <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), ...) {
    UseMethod("char_extracttags")
}
        
#' @noRd
#' @export
char_extracttags.character <- function(x, pattern, valuetype = c("glob", "regex", "fixed"), ...) {
    
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

