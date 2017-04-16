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
                           use_docvars = TRUE, 
                           ...) {
    UseMethod("corpus_segment")
}

#' @noRd
#' @rdname corpus_segment
#' @export    
corpus_segment.corpus <- function(x, what = c("sentences", "paragraphs", "tokens", "other"), 
                                  delimiter = NULL,
                                  valuetype = c("regex", "fixed", "glob"),
                                  use_docvars = TRUE, 
                           ...) {
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    
    temp <- segment_texts(texts(x), what, delimiter, valuetype, ...)

    # get the relevant function call
    commands <- as.character(sys.calls())
    commands <- commands[stri_detect_regex(commands, "segment\\.corpus")]
    
    # create the new corpus
    result <- corpus(unlist(temp, use.names = FALSE), metacorpus = list(source = metacorpus(x, "source"),
                                                                        notes = commands))
    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(docvars(x))) {
        result[[names(docvars(x))]] <- docvars(x)[attr(temp, 'docid'),,drop = FALSE]
    }
    if (what == 'tags') {
        docvars(result, 'tag') <- names(temp)
    }
    docvars(result, 'docid') <- attr(temp, 'docid')
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
                                   use_docvars = TRUE, 
                                   ...) {
        
    if (!all(is.character(x)))
        stop("x must be of character type")
    
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    
    result <- segment_texts(x, what, delimiter, valuetype, ...)
    attributes(result) <- NULL
    return(result)
}

# internal function for char_segment and corpus_segment
segment_texts <- function(x, what, delimiter, valuetype, ...){
    
    if (what %in% c('tokens', 'sentences')) {
        if (!is.null(delimiter))
            warning("delimiter is only used for 'other'")
        delimiter <- NULL
    } else if (what == 'paragraphs') {
        if (!is.null(delimiter)) 
            warning("delimiter is only used for 'other'")
        delimiter <- "\\n{2}"
        valuetype <- "regex"
    } else if (what == 'other') {
        if (is.null(delimiter))
            stop("You must supply a delimiter value for 'other'")
    }

    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!any(stringi::stri_detect_charclass(delimiter, c("[*?]"))))
            valuetype <- "fixed"
        else {
            delimiter <- paste0(utils::glob2rx(delimiter), collapse = '|')
            valuetype <- "regex"
        }
    }
    
    if (what == "tokens") {
        temp <- tokens_word(x, ...)
    } else if (what == "sentences") {
        temp <- tokens_sentence(x, ...)
    } else if (what == 'tags') {
        temp <- stringi::stri_replace_all_regex(x, "(##\\w+\\b)", "\v$1") # insert contrl character
        temp <- stringi::stri_split_fixed(temp, pattern = "\v", omit_empty = TRUE)
    } else {
        if (valuetype == "fixed") {
            temp <- stringi::stri_split_fixed(x, pattern = delimiter, omit_empty = TRUE)
        } else {
            temp <- stringi::stri_split_regex(x, pattern = delimiter, omit_empty = TRUE)
        }
    }
    
    docid <- rep(seq_along(x), lengths(temp))
    result <- unlist(temp, use.names = FALSE)
    if (what == 'tags') {
        # to make names ##INTRO, ##DOC1, #DOC2 ...
        tags <- stringi::stri_extract_first_regex(result, "(##\\w+\\b)")
        result <- stringi::stri_replace_first_fixed(result, tags, '')
        result <- stringi::stri_trim_both(result)
        names(result) <- tags
    } else {
        # to make names doc1.1, doc1.2, doc2.1, ...
        result <- stringi::stri_trim_both(result)
        names(result) <- paste0(rep(names(x), lengths(temp)), ".", unlist(lapply(lengths(temp), seq_len)))
    }
    attr(result,'docid') <- docid
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
