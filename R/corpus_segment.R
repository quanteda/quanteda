#' segment texts on a pattern match
#' 
#' Segment corpus text(s) or a character vector, splitting
#' on a pattern match.  This is useful for breaking the texts into smaller
#' documents based on a regular pattern (such as a speaker identifier in a
#' transcript) or a user-supplied annotation (a "tag").
#' 
#' For segmentation into syntactic units defined by the locale (such as 
#' sentences), use \code{\link{corpus_reshape}} instead.  In cases where more 
#' fine-grained segmentation is needed, such as that based on commas or 
#' semi-colons (phrase delimiters within a sentence), 
#' \code{\link{corpus_segment}} offers greater user control than 
#' \code{\link{corpus_reshape}}.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param extract_pattern extracts matched patterns from the texts and save in docvars if
#'   \code{TRUE}
#' @param pattern_position either \code{"before"} or \code{"after"}, depending 
#'   on whether the pattern precedes the text (as with a tag) or follows the 
#'   text (as with punctuation delimiters)
#' @param use_docvars if \code{TRUE}, repeat the docvar values for each 
#'   segmented text; if \code{FALSE}, drop the docvars in the segmented corpus. 
#'   Dropping the docvars might be useful in order to conserve space or if these
#'   are not desired for the segmented corpus.
#' @return \code{corpus_segment} returns a corpus of segmented texts
#' @keywords corpus
#' @section Boundaries and segmentation explained: The \code{pattern} acts as a
#'   boundary delimiter that defines the segmentation points for splitting a
#'   text into new "document" units.  Boundaries are always defined as the
#'   pattern matches, plus the end and beginnings of each document.  The new
#'   "documents" that are created following the segmenation will then be the
#'   texts found between boundaries.
#'   
#'   The pattern itself will be saved as a new document variable named 
#'   \code{pattern}.  This is most useful when segmenting a text according to 
#'   tags such as names in a transcript, section titles, or user-supplied 
#'   annotations.  If the beginning of the file precedes a pattern match, then 
#'   the extracted text will have a \code{NA} for the extracted \code{pattern} 
#'   document variable (or when \code{pattern_position = "after"}, this will be 
#'   true for the text split between the last pattern match and the end of the 
#'   document).
#'   
#'   To extract syntactically defined sub-document units such as sentences and 
#'   paragraphs, use \code{\link{corpus_reshape}} instead.
#' @section Using patterns: One of the most common uses for
#'   \code{corpus_segment} is to partition a corpus into sub-documents using
#'   tags.  The default pattern value is designed for a user-annotated tag that
#'   is a term begining with double "hash" signs, followed by a whitespace, for
#'   instance as \code{##INTRODUCTION The text}.
#'   
#'   Glob and fixed pattern types use a whitespace character to signal the end 
#'   of the pattern.
#'   
#'   For more advanced pattern matches that could include whitespace or 
#'   newlines, a regex pattern type can be used, for instance a text such as
#'   
#'   \code{Mr. Smith: Text} \cr \code{Mrs. Jones: More text}
#'   
#'   could have as \code{pattern = "\\\\b[A-Z].+\\\\.\\\\s[A-Z][a-z]+:"}, which
#'   would catch the title, the name, and the colon.
#'   
#'   For custom boundary delimitation using punctuation characters that come 
#'   come at the end of a clause or sentence (such as \code{,} and\code{.}, 
#'   these can be specified manually and \code{pattern_position} set to 
#'   \code{"after"}. To keep the punctuation characters in the text (as with 
#'   sentence segmentation), set \code{extract_pattern = FALSE}.  (With most tag
#'   applications, users will want to remove the patterns from the text, as they
#'   are annotations rather than parts of the text itself.)
#' @seealso \code{\link{corpus_reshape}}, for segmenting texts into pre-defined 
#'   syntactic units such as sentences, paragraphs, or fixed-length chunks
#' @examples
#' ## segmenting a corpus
#' 
#' # segmenting a corpus using tags
#' corp <- corpus(c("##INTRO This is the introduction.
#'                   ##DOC1 This is the first document.  Second sentence in Doc 1.
#'                   ##DOC3 Third document starts here.  End of third document.",
#'                  "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' corp_seg <- corpus_segment(corp, "##*")
#' cbind(texts(corp_seg), docvars(corp_seg), metadoc(corp_seg))
#' 
#' # segmenting a transcript based on speaker identifiers
#' corp2 <- corpus("Mr. Smith: Text.\nMrs. Jones: More text.\nMr. Smith: I'm speaking, again.")
#' corp_seg2 <- corpus_segment(corp2, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:",
#'                             valuetype = "regex")
#' cbind(texts(corp_seg2), docvars(corp_seg2), metadoc(corp_seg2))
#' 
#' # segmenting a corpus using crude end-of-sentence segmentation
#' corp_seg3 <- corpus_segment(corp, pattern = ".", valuetype = "fixed", 
#'                             pattern_position = "after", extract_pattern = FALSE)
#' cbind(texts(corp_seg3), docvars(corp_seg3), metadoc(corp_seg3))
#' 
#' @import stringi
#' @export
corpus_segment <- function(x, pattern = "##*",
                           valuetype = c("glob", "regex", "fixed"),
                           extract_pattern = TRUE,
                           pattern_position = c("before", "after"),
                           use_docvars = TRUE) {
    UseMethod("corpus_segment")
}

#' @noRd
#' @rdname corpus_segment
#' @export    
corpus_segment.corpus <- function(x, pattern = "##*",
                                  valuetype = c("glob", "regex", "fixed"),
                                  extract_pattern = TRUE,
                                  pattern_position = c("before", "after"),
                                  use_docvars = TRUE) {
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    vars <- docvars(x)
    
    # get the relevant function call
    commands <- as.character(sys.calls())
    commands <- commands[stri_detect_regex(commands, "segment\\.corpus")]
    
    temp <- segment_texts(texts(x), pattern = pattern, valuetype = valuetype, 
                          extract_pattern = extract_pattern, 
                          pattern_position = pattern_position,
                          omit_empty = !extract_pattern)

    # create the new corpus
    result <- corpus(temp$texts, docnames = rownames(temp),
                     metacorpus = list(source = metacorpus(x, "source"),
                                       notes = commands))
    
    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(vars)) {
        vars <- select_fields(vars, "user")[temp$docid,,drop = FALSE]
        rownames(vars) <- rownames(temp)
        docvars(result) <- vars
    }
    docvars(result, '_document') <- temp$docname
    docvars(result, '_docid') <- temp$docid
    docvars(result, '_segid') <- temp$segid
    if (extract_pattern) docvars(result, "pattern") <- temp$pattern
    settings(result, "units") <- 'other'
    
    return(result)
}


#' @rdname corpus_segment
#' @param remove_pattern removes matched patterns from the texts if \code{TRUE}
#' @export
#' @examples
#' ## segmenting a character vector
#' 
#' # segment into paragraphs and removing the "- " bullet points
#' cat(data_char_ukimmig2010[4])
#' char_segment(data_char_ukimmig2010[4], 
#'              pattern = "\\n\\n(\\-\\s){0,1}", valuetype = "regex", remove_pattern = TRUE)
#' 
#' # segment a text into clauses
#' txt <- c(d1 = "This, is a sentence?  You: come here.", d2 = "Yes, yes, okay.")
#' char_segment(txt, pattern = "\\p{P}", valuetype = "regex", 
#'              pattern_position = "after", remove_pattern = FALSE)
#' @keywords character
#' @return \code{char_segment} returns a character vector of segmented texts
char_segment <- function(x, pattern = "##*",
                         valuetype = c("glob", "regex", "fixed"),
                         remove_pattern = TRUE,
                         pattern_position = c("before", "after")) {
    UseMethod("char_segment")
}
        
#' @noRd
#' @export
char_segment.character <- function(x, pattern = "##*",
                                   valuetype = c("glob", "regex", "fixed"),
                                   remove_pattern = TRUE,
                                   pattern_position = c("before", "after")) {
    
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    
    temp <- segment_texts(x, pattern, valuetype, remove_pattern, pattern_position)
    result <- temp$texts
    if (!is.null(names(x)))
        names(result) <- rownames(temp)
    return(result)
}

# internal function for char_segment and corpus_segment
segment_texts <- function(x, pattern = NULL, valuetype = "regex", 
                          extract_pattern = FALSE, pattern_position = "after", 
                          omit_empty = TRUE, what = "other", ...){
    
    docname <- names(x)
    
    # normalize EOL
    x <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh
    
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
            if (pattern_position == "after") {
                temp <- stri_replace_all_fixed(temp, pattern, stri_c(pattern, "\uE000"))
            } else {
                temp <- stri_replace_all_fixed(temp, pattern, stri_c("\uE000", pattern))
            }
        } else {
            if (pattern_position == "after") {
                temp <- stri_replace_all_regex(temp, pattern, "$0\uE000")
            } else {
                temp <- stri_replace_all_regex(temp, pattern, "\uE000$0")
            }
        }
        temp <- stri_split_fixed(temp, pattern = "\uE000", omit_empty = omit_empty)
    }
    
    result <- data.frame(texts = unlist(temp, use.names = FALSE), stringsAsFactors = FALSE)
    result$docid <- rep(seq_len(length(temp)), lengths(temp))
    if (!is.null(docname)) result$docname = rep(docname, lengths(temp))
    
    if (!is.null(pattern)) {    
        if (extract_pattern) {
            if (valuetype == "fixed") {
                result$pattern <- stri_extract_first_fixed(result$texts, pattern)
            } else {
                result$pattern <- stri_extract_first_regex(result$texts, pattern)
            }
            if (pattern_position == "after") {
                result$texts <- stri_replace_last_fixed(result$texts, result$pattern, '', vectorize_all = TRUE)
            } else {
                result$texts <- stri_replace_first_fixed(result$texts, result$pattern, '', vectorize_all = TRUE)
            }
        }
    }
    
    result$texts <- stri_trim_both(result$texts)
    result <- result[!is.na(result$texts),]
    if (omit_empty) result <- result[result$texts != '',] # remove empty documents 
    result$segid <- unlist(lapply(rle(result$docid)$lengths, seq_len))

    if (!is.null(docname)) {
        # to make names doc1.1, doc1.2, doc2.1, ...
        rownames(result) <- stri_c(result$docname, ".", result$segid)
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
