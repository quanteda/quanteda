#' segment texts into component elements
#' 
#' Segment corpus text(s) or a character vector into tokens, sentences, 
#' paragraphs, or other sections. \code{segment} works on a character vector or 
#' corpus object, and allows the patterns to be user-defined.  This is useful 
#' for breaking the texts of a corpus into smaller documents based on sentences,
#' or based on a user defined "tag" pattern.  See Details.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @param pattern  pattern defined as a \code{\link{regex}} for 
#'   segmentation; only relevant for \code{what = "paragraphs"} (where the 
#'   default is two newlines), \code{"tags"} (where the default is a tag 
#'   preceded by two pound or "hash" signs \code{##}), and \code{"other"}.
#' @inheritParams valuetype
#' @param remove_pattern removes matched patterns from the texts if \code{TRUE}.
#' @param position specify position to split texts for \code{what = "other"}
#' @param use_docvars (for corpus objects only) if \code{TRUE}, repeat the docvar 
#'   values for each segmented text; if \code{FALSE}, drop the docvars in the 
#'   segmented corpus. Dropping the docvars might be useful in order to conserve
#'   space or if these are not desired for the segmented corpus.
#' @param ... provides additional arguments passed to internal functions
#' @return \code{corpus_segment} returns a corpus of segmented texts
#' @details Tokens are delimited by separators.  For tokens and sentences, these
#'   are determined by the tokenizer behaviour in \code{\link{tokens}}.
#'   
#'   For paragraphs, the default is two carriage returns, although this could be
#'   changed to a single carriage return by changing the value of 
#'   \code{pattern} to \code{"\\\n{1}"} which is the R version of the 
#'   \code{\link{regex}} for one newline character.  (You might need this if the
#'   document was created in a word processor, for instance, and the lines were 
#'   wrapped in the window rather than being hard-wrapped with a newline 
#'   character.)
#' @keywords corpus
#' @section Using patterns:
#'   One of the most common uses for \code{corpus_segment} is to 
#'   partition a corpus into sub-documents using tags.  By default, the tag 
#'   value is any word that begins with a double "hash" sign and is followed by 
#'   a whitespace.  This can be modified but be careful to use the syntax for
#'   the trailing word boundary (\code{\\\\b})
#'   
#'   The default values for \code{pattern} are, according to valuetype:
#'   \describe{
#'   \item{paragraphs}{\code{"\\\\n{2}"}, \link[=regex]{regular expression} 
#'     meaning two newlines.  If you wish to define a paragaph as a single 
#'     newline, change the 2 to a 1.}
#'   \item{other}{No default; user must supply one.}
#'   \item{tokens, sentences}{patterns do not apply to these, and a warning
#'     will be issued if you attempt to supply one.}
#'   }
#'   
#'   patterns may be defined for different \link[=valuetype]{valuetypes} 
#'   but these may produce unexpected results, for example the lack of the
#'   ability in a "glob" expression to define the word boundaries.
#' @seealso \code{\link{corpus_reshape}}, \code{\link{tokens}}
#' @examples
#' ## segmenting a corpus
#' 
#' corp <- 
#' corpus(c("##INTRO This is the introduction.
#'           ##DOC1 This is the first document.  Second sentence in Doc 1.
#'           ##DOC3 Third document starts here.  End of third document.",
#'          "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' corp_seg <- corpus_segment(corp, "##[A-Z0-9]+", valuetype = "regex", position = "before")
#' texts(corp_seg)
#'
#' corp_seg2 <- corpus_segment(corp, ".", valuetype = "fixed", position = "after")
#' texts(corp_seg2)
#' 
#' @import stringi
#' @export
corpus_segment <- function(x, pattern,
                           valuetype = c("regex", "fixed", "glob"),
                           remove_pattern = FALSE,
                           position = c("after", "before"),
                           use_docvars = TRUE, 
                           ...) {
    UseMethod("corpus_segment")
}

#' @noRd
#' @rdname corpus_segment
#' @export    
corpus_segment.corpus <- function(x, pattern,
                                  valuetype = c("regex", "fixed", "glob"),
                                  remove_pattern = FALSE,
                                  position = c("after", "before"),
                                  use_docvars = TRUE, 
                                  ...) {
    
    valuetype <- match.arg(valuetype)
    position <- match.arg(position)
    vars <- docvars(x)
    
    temp <- segment_texts(texts(x), pattern, valuetype, remove_pattern, position, ...)

    # get the relevant function call
    commands <- as.character(sys.calls())
    commands <- commands[stri_detect_regex(commands, "segment\\.corpus")]
    
    # create the new corpus
    result <- corpus(temp, metacorpus = list(source = metacorpus(x, "source"),
                                             notes = commands))
    settings(result, "units") <- 'other'

    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(vars)) {
        rownames(vars) <- NULL # faster to repeat rows without rownames
        vars <- select_fields(vars, "user")[attr(temp, 'docid'),,drop = FALSE]
        rownames(vars) <- stri_c(attr(temp, 'document'), '.', attr(temp, 'segid'), sep = '')
        docvars(result) <- vars
    }
    docvars(result, '_document') <- attr(temp, 'document')
    docvars(result, '_docid') <- attr(temp, 'docid')
    docvars(result, '_segid') <- attr(temp, 'segid')
    
    return(result)
}


#' @rdname corpus_segment
#' @export
#' @examples
#' ## segmenting a character object
#' 
#' # segment into paragraphs
#' char_segment(data_char_ukimmig2010[3:4], pattern = "\\n\\n", valuetype = "regex", remove_pattern = TRUE)
#' 
#' # segment a text into sentences
#' char_segment(data_char_ukimmig2010[3:4], pattern = "\\p{P}", valuetype = "regex", remove_pattern = FALSE)
#' @keywords character
#' @return \code{char_segment} returns a character vector of segmented texts
char_segment <- function(x, pattern,
                         valuetype = c("regex", "fixed", "glob"),
                         remove_pattern = FALSE,
                         position = c("after", "before"),
                         ...) {
    UseMethod("char_segment")
}
        
#' @noRd
#' @export
char_segment.character <- function(x, pattern,
                                   valuetype = c("regex", "fixed", "glob"),
                                   remove_pattern = FALSE,
                                   position = c("after", "before"),
                                   ...) {
    
    valuetype <- match.arg(valuetype)
    position <- match.arg(position)
    
    # normalize EOL
    temp <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    temp <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh
    
    names(temp) <- names(x)
    result <- segment_texts(temp, pattern, valuetype, remove_pattern, position, ...)
    result <- result[result!='']
    
    attr(result, 'tag') <- NULL
    attr(result, 'document') <- NULL
    attr(result, 'docid') <- NULL
    attr(result, 'segid') <- NULL
    
    return(result)
}

# internal function for char_segment and corpus_segment
segment_texts <- function(x, pattern = NULL, valuetype = "regex", 
                          remove_pattern = FALSE, position = "after", 
                          omit_empty = TRUE, what = "other", ...){
    
    # use preset regex pattern
    if (what == 'paragraphs') {
        pattern <- "\\n\\n"
        valuetype <- "regex"
    }
    
    if (is.null(pattern)) {
        
        if (what == "tokens") {
            temp <- as.list(tokens(x, ...))
        } else if (what == "sentences") {
            temp <- as.list(tokens(x, what = "sentence", ...))
        }
        
    } else {
        
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
        
        temp <- stri_trim_both(x)
        if (valuetype == "fixed") {
            if (remove_pattern) {
                temp <- stri_replace_all_fixed(temp, pattern, "\uE000")
            } else {
                if (position == "after") {
                    temp <- stri_replace_all_fixed(temp, pattern, stri_c(pattern, "\uE000"))
                } else {
                    temp <- stri_replace_all_fixed(temp, pattern, stri_c("\uE000", pattern))
                }
            }
        } else {
            if (remove_pattern) {
                temp <- stri_replace_all_regex(temp, pattern, "\uE000")
            } else {
                if (position == "after") {
                    temp <- stri_replace_all_regex(temp, pattern, "$0\uE000")
                } else {
                    temp <- stri_replace_all_regex(temp, pattern, "\uE000$0")
                }
            }
        }
        temp <- stri_split_fixed(temp, pattern = "\uE000", omit_empty = omit_empty)
    }
    
    n <- lengths(temp)
    result <- unlist(temp, use.names = FALSE)
    result <- stri_trim_both(result)
    attr(result, 'document') <- rep(names(x), n)
    attr(result, 'docid') <- rep(seq_along(x), n)
    attr(result, 'segid') <- unlist(lapply(n, seq_len), use.names = FALSE)

    if (!is.null(names(x))) {
        # to make names doc1.1, doc1.2, doc2.1, ...
        names(result) <- stri_c(attr(result, 'document'), ".", attr(result,'segid'))
    }
    
    return(result)
}

#' segment: deprecated function
#' 
#' See \code{\link{corpus_segment}}
#' @param x object to be segmented
#' @param ... additional arguments passed to  \code{\link{corpus_segment}}.
#' @export
#' @keywords internal deprecated
segment <- function(x, ...) {
    .Deprecated("corpus_segment")
    UseMethod("segment")
}

#' @rdname segment
#' @keywords internal deprecated
#' @export
segment.character <- function(x, ...) {
    char_segment(x, ...)
}

#' @rdname segment
#' @keywords internal deprecated
#' @export
segment.corpus <- function(x, ...) {
    corpus_segment(x, ...)
}
