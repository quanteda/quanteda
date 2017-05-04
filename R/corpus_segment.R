#' segment texts into component elements
#' 
#' Segment corpus text(s) or a character vector into tokens, sentences, 
#' paragraphs, or other sections. \code{segment} works on a character vector or 
#' corpus object, and allows the delimiters to be user-defined.  This is useful 
#' for breaking the texts of a corpus into smaller documents based on sentences,
#' or based on a user defined "tag" pattern.  See details.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @param what unit of segmentation.  Current options are  
#'   \code{"sentences"} (default), \code{"paragraphs"}, \code{"tokens"}, 
#'   \code{"tags"}, and \code{"other"}.
#'   
#'   Segmenting on \code{"other"} allows segmentation of a text on any 
#'   user-defined value, and must be accompanied by the \code{delimiter} 
#'   argument.  Segmenting on \code{"tags"} performs the same function but 
#'   preserves the tags as a document variable in the segmented corpus.
#' @param delimiter  delimiter defined as a \code{\link{regex}} for 
#'   segmentation; only relevant for \code{what = "paragraphs"} (where the 
#'   default is two newlines), \code{"tags"} (where the default is a tag 
#'   preceded by two pound or "hash" signs \code{##}), and \code{"other"}.
#' @inheritParams valuetype
#' @param omit_empty if \code{TRUE}, empty texts are removed 
#' @param use_docvars (for corpus objects only) if \code{TRUE}, repeat the docvar 
#'   values for each segmented text; if \code{FALSE}, drop the docvars in the 
#'   segmented corpus. Dropping the docvars might be useful in order to conserve
#'   space or if these are not desired for the segmented corpus.
#' @param ... provides additional arguments passed to \code{\link{tokens}}, if 
#'   \code{what = "tokens"} is used
#' @return \code{corpus_segment} returns a corpus of segmented texts, with a 
#'   \code{tag} docvar if \code{what = "tags"}.
#' @note Does not currently record document segments if segmenting a multi-text 
#'   corpus into smaller units. For this, use \link{corpus_reshape} instead.
#' @details Tokens are delimited by separators.  For tokens and sentences, these
#'   are determined by the tokenizer behaviour in \code{\link{tokens}}.
#'   
#'   For paragraphs, the default is two carriage returns, although this could be
#'   changed to a single carriage return by changing the value of 
#'   \code{delimiter} to \code{"\\\n{1}"} which is the R version of the 
#'   \code{\link{regex}} for one newline character.  (You might need this if the
#'   document was created in a word processor, for instance, and the lines were 
#'   wrapped in the window rather than being hard-wrapped with a newline 
#'   character.)
#' @keywords corpus
#' @section Using delimiters:
#'   One of the most common uses for \code{corpus_segment} is to 
#'   partition a corpus into sub-documents using tags.  By default, the tag 
#'   value is any word that begins with a double "hash" sign and is followed by 
#'   a whitespace.  This can be modified but be careful to use the syntax for
#'   the trailing word boundary (\code{\\\\b})
#'   
#'   The default values for \code{delimiter} are, according to valuetype:
#'   \describe{
#'   \item{paragraphs}{\code{"\\\\n{2}"}, \link[=regex]{regular expression} 
#'     meaning two newlines.  If you wish to define a paragaph as a single 
#'     newline, change the 2 to a 1.}
#'   \item{tags}{\code{"##\\\\w+\\\\b"}, a \link[=regex]{regular expression} 
#'     meaning two "hash" characters followed by any number of word characters
#'     followed by a word boundary (a whitespace or the end of the text).}
#'   \item{other}{No default; user must supply one.}
#'   \item{tokens, sentences}{Delimiters do not apply to these, and a warning
#'     will be issued if you attempt to supply one.}
#'   }
#'   
#'   Delimiters may be defined for different \link[=valuetype]{valuetypes} 
#'   but these may produce unexpected results, for example the lack of the
#'   ability in a "glob" expression to define the word boundaries.
#' @seealso \code{\link{corpus_reshape}}, \code{\link{tokens}}
#' @examples
#' ## segmenting a corpus
#' 
#' testCorpus <- 
#' corpus(c("##INTRO This is the introduction.
#'           ##DOC1 This is the first document.  Second sentence in Doc 1.
#'           ##DOC3 Third document starts here.  End of third document.",
#'          "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' # add a docvar
#' testCorpus[["serialno"]] <- paste0("textSerial", 1:ndoc(testCorpus))
#' testCorpusSeg <- corpus_segment(testCorpus, "tags")
#' summary(testCorpusSeg)
#' texts(testCorpusSeg)
#' # segment a corpus into sentences
#' segmentedCorpus <- corpus_segment(corpus(data_char_ukimmig2010), "sentences")
#' summary(segmentedCorpus)
#' 
#' @author Kenneth Benoit
#' @export
corpus_segment <- function(x, what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                           delimiter = NULL,
                           valuetype = c("regex", "fixed", "glob"),
                           omit_empty = TRUE,
                           use_docvars = TRUE, 
                           ...) {
    UseMethod("corpus_segment")
}

#' @noRd
#' @rdname corpus_segment
#' @export    
corpus_segment.corpus <- function(x, what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                                  delimiter = NULL,
                                  valuetype = c("regex", "fixed", "glob"),
                                  omit_empty = TRUE,
                                  use_docvars = TRUE, 
                           ...) {
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    
    temp <- segment_texts(texts(x), what, delimiter, valuetype, omit_empty, ...)

    # get the relevant function call
    commands <- as.character(sys.calls())
    commands <- commands[stri_detect_regex(commands, "segment\\.corpus")]
    
    # create the new corpus
    result <- corpus(temp, metacorpus = list(source = metacorpus(x, "source"),
                                             notes = commands))
    settings(result, "units") <- what
    
    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(docvars(x))) {
        result[[names(docvars(x))]] <- docvars(x)[attr(temp, 'docid'),,drop = FALSE]
    }
    if (what == 'tags') {
        docvars(result, 'tag') <- attr(temp, 'tag')
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
#' # same as tokenize()
#' identical(as.character(tokens(data_char_ukimmig2010)), 
#'           as.character(char_segment(data_char_ukimmig2010, what = "tokens")))
#' 
#' # segment into paragraphs
#' char_segment(data_char_ukimmig2010[3:4], "paragraphs")
#' 
#' # segment a text into sentences
#' segmentedChar <- char_segment(data_char_ukimmig2010, "sentences")
#' segmentedChar[3]
#' @keywords character
#' @return \code{corpus_segment} returns a character vector of segmented texts
char_segment <- function(x, 
                         what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                         delimiter = NULL,
                         valuetype = c("regex", "fixed", "glob"),
                         omit_empty = TRUE,
                         use_docvars = TRUE, 
                         ...) {
    UseMethod("char_segment")
}
        
#' @noRd
#' @export
char_segment.character <- function(x, 
                                   what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                                   delimiter = NULL,
                                   valuetype = c("regex", "fixed", "glob"),
                                   omit_empty = TRUE,
                                   use_docvars = TRUE, 
                                   ...) {
        
    if (!all(is.character(x)))
        stop("x must be of character type")
    
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    names_org <- names(x)
    
    # normalize EOL
    x <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh
    
    names(x) <- names_org
    result <- segment_texts(x, what, delimiter, valuetype, omit_empty, ...)
    result <- result[result!='']
    
    attr(result, 'tag') <- NULL
    attr(result, 'document') <- NULL
    attr(result, 'docid') <- NULL
    attr(result, 'segid') <- NULL
    
    return(result)
}

# internal function for char_segment and corpus_segment
segment_texts <- function(x, what, delimiter, valuetype, omit_empty, ...){
    
    names_org <- names(x)
    
    if (what %in% c('tokens', 'sentences')) {
        if (!is.null(delimiter)) warning("delimiter is only used for 'paragraphs', 'tags' or 'other'")
        delimiter <- NULL
    } else if (what == 'paragraphs') {
        if (is.null(delimiter)) {
            delimiter <- "\\n\\n"
            valuetype <- "regex"
        }
    } else if (what == 'tags') {
        if (is.null(delimiter)) {
            delimiter <- "##\\w+\\b"
            valuetype <- "regex"
        }
    } else if (what == 'other') {
        if (is.null(delimiter)) {
            stop("You must supply a delimiter value for 'other'")
        }
    }

    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!any(stri_detect_charclass(delimiter, c("[*?]")))) {
            valuetype <- "fixed"
        } else {
            regex <- escape_regex(delimiter)
            regex <- stri_replace_all_fixed(regex, '*', '(\\S*)')
            regex <- stri_replace_all_fixed(regex, '?', '(\\S)')
            delimiter <- stri_c(regex, collapse = '|')
            valuetype <- "regex"
        }
    }
    
    if (what == "tokens") {
        temp <- tokens_word(x, ...)
    } else if (what == "sentences") {
        temp <- tokens_sentence(x, ...)
    } else if (what == 'tags') {
        temp <- stri_replace_all_regex(x, delimiter, "\UE000$0") # insert PUA character
        temp <- stri_split_fixed(temp, pattern = "\UE000", omit_empty = omit_empty)
        # remove elements to be empty
        temp <- lapply(temp, function(x) x[stri_replace_first_regex(x, '^\\s+$', '') != ''])
    } else {
        if (valuetype == "fixed") {
            temp <- stri_replace_all_fixed(x, delimiter, stri_c(delimiter, "\UE000"))
            temp <- stri_split_fixed(x, pattern = "\UE000", omit_empty = omit_empty)
        } else {
            temp <- stri_replace_all_regex(x, delimiter, "$0\UE000")
            temp <- stri_split_fixed(temp, pattern = "\UE000", omit_empty = omit_empty)
        }
    }

    result <- unlist(temp, use.names = FALSE)
    
    if (what == 'tags') {
        tag <- stri_extract_first_regex(result, delimiter)
        result <- stri_replace_first_fixed(result, tag, '')
        result <- stri_trim_both(result)
        attr(result,'tag') <- tag
    } else {
        result <- stri_trim_both(result)
    }
    
    n_segment <- lengths(temp)
    attr(result, 'document') <- rep(names_org, n_segment)
    attr(result,'docid') <- rep(seq_along(x), n_segment)
    
    id_segment <- unlist(lapply(n_segment, seq_len), use.names = FALSE)
    attr(result,'segid') <- id_segment

    if (!is.null(names_org)) {
        # to make names doc1.1, doc1.2, doc2.1, ...
        names(result) <- stri_c(rep(names_org, n_segment), ".", id_segment)
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
