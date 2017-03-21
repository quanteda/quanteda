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


#' segment texts into component elements
#' 
#' Segment corpus text(s) or a character vector into tokens, sentences, 
#' paragraphs, or other sections. \code{segment} works on a character vector or 
#' corpus object, and allows the delimiters to be user-defined.  This is useful 
#' for breaking the texts of a corpus into smaller documents based on sentences,
#' or based on a user defined "tag" pattern.  See details.
#' @param x character or \link{corpus} object whose texts will be segmented
#' @param what unit of segmentation.  Current options are \code{"tokens"} 
#'   (default), \code{"sentences"}, \code{"paragraphs"}, \code{"tags"}, and 
#'   \code{"other"}.  Segmenting on \code{other} allows segmentation of a text 
#'   on any user-defined value, and must be accompanied by the \code{delimiter} 
#'   argument.  Segmenting on \code{tags} performs the same function but 
#'   preserves the tags as a document variable in the segmented corpus.
#' @param delimiter  delimiter defined as a \code{\link{regex}} for 
#'   segmentation; only relevant for \code{what = "paragraphs"} (where the
#'   default is two newlines) and \code{what = "tags"} (where the default is a
#'   tag preceded by two pound or "hash" signs \code{##}).  Delimiter has no
#'   effect for segmentation into tokens or sentences
#' @inheritParams valuetype
#' @param use_docvars (for corpus objects) if \code{TRUE}, repeat the docvar
#'   values for each segmented text; if \code{FALSE}, drop the docvars in the
#'   segmented corpus. Dropping the docvars might be useful in order to conserve
#'   space or if these are not desired for the segmented corpus.
#' @param ... provides additional arguments passed to \code{\link{tokens}}, if 
#'   \code{what = "tokens"} is used
#' @return A corpus of segmented texts.
#' @note Does not currently record document segments if segmenting a multi-text 
#'   corpus into smaller units. For this, use \link{corpus_reshape} instead.
#' @details Tokens are delimited by separators.  For tokens and sentences, these
#' are determined by the tokenizer behaviour in \code{\link{tokens}}.
#'   
#'   For paragraphs, the default is two carriage returns, although this could be
#'   changed to a single carriage return by changing the value of 
#'   \code{delimiter} to \code{"\\\n{1}"} which is the R version of the 
#'   \code{\link{regex}} for one newline character.  (You might need this if the
#'   document was created in a word processor, for instance, and the lines were 
#'   wrapped in the window rather than being hard-wrapped with a newline 
#'   character.)
#' @keywords corpus
#' @examples
#' testCorpus <- corpus(c("##INTRO This is the introduction.
#'                         ##DOC1 This is the first document.  Second sentence in Doc 1.
#'                         ##DOC3 Third document starts here.  End of third document.",
#'                        "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' # add a docvar
#' testCorpus[["serialno"]] <- paste0("textSerial", 1:ndoc(testCorpus))
#' testCorpusSeg <- corpus_segment(testCorpus, "tags")
#' summary(testCorpusSeg)
#' texts(testCorpusSeg)
#' # segment a corpus into sentences
#' segmentedCorpus <- corpus_segment(corpus(data_char_ukimmig2010), "sentences")
#' summary(segmentedCorpus)
#' 
#' @export
corpus_segment <- function(x, what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                           delimiter = switch(what, paragraphs = "\\n{2}", tags = "##\\w+\\b", NULL),
                           valuetype = c("regex", "fixed", "glob"),
                           use_docvars = TRUE, 
                           ...) {
    UseMethod("corpus_segment")
}

#' @noRd
#' @rdname corpus_segment
#' @export    
corpus_segment.corpus <- function(x, what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                                  delimiter = switch(what, paragraphs = "\\n{2}", tags = "##\\w+\\b", NULL),
                                  valuetype = c("regex", "fixed", "glob"),
                                  use_docvars = TRUE, 
                           ...) {
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    # automatically detect and override valuetype
    if (!is.null(delimiter) && gsub("[*?]|\\w|\\s", "", delimiter) != "" & valuetype != "regex") {
        warning("delimiter looks like it contains a regex", noBreaks. = TRUE)
    } else if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(delimiter, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- vapply(delimiter, utils::glob2rx, character(1), USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    segTxt <- lapply(texts(x), function(y) unname(char_segment(y, what, delimiter, valuetype = valuetype, ...)))
    names(segTxt) <- paste0(names(segTxt), ".")
    
    newCorpus <- corpus(unlist(segTxt),
                        metacorpus = list(source = metacorpus(x, "source"),
                                          notes = paste0("corpus_segment(", match.call(), ")")))
    
    if (what == "tags") {
        tagIndex <- gregexpr(delimiter, cattxt <- paste0(texts(x), collapse = ""), 
                             fixed = (valuetype == "fixed"))[[1]]
        tags <- character()
        length(tags) <- ndoc(newCorpus)
        for (i in seq_along(tagIndex))
            tags[i] <- substr(cattxt, start = tagIndex[i],
                              stop = tagIndex[i] + attr(tagIndex, "match.length")[i] - 1)
        # remove white space at both ends
        tags <- stringi::stri_trim_both(tags)
        # add tag as a docvar
        docvars(newCorpus, "tag") <- tags
    }
    
    # add repeated versions of remaining docvars
    if (!is.null(docvars(x)) & use_docvars) {
        newCorpus[[names(docvars(x))]] <- lapply(docvars(x), rep, lengths(segTxt))
    }

    newCorpus
}


#' @rdname corpus_segment
#' @export
#' @examples
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
char_segment <- function(x, what = c("sentences", "paragraphs", "tokens", "tags", "other"), 
                         delimiter = switch(what, paragraphs = "\\n{2}", tags = "##\\w+\\b", NULL),
                         valuetype = c("regex", "fixed", "glob"),
                         use_docvars = TRUE, 
                         ...) {
   
    if (!all(is.character(x)))
        stop("x must be of character type")
    
    what <- match.arg(what)
    if (what == "other" & is.null(delimiter))
        stop("For type other, you must supply a delimiter value.")
        
    valuetype <- match.arg(valuetype)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(delimiter, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- vapply(delimiter, utils::glob2rx, character(1), USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (!is.null(names(x)))
        names(x) <- paste0(names(x), ".")
    
    if (what == "tokens") {
        return(as.character(tokens(x, ...), use.names = TRUE))
    } else if (what == "sentences") {
        return(as.character(tokens(x, what = "sentence", ...), use.names = TRUE))
    } else {
        if (valuetype == "fixed") {
            result <- stringi::stri_split_fixed(x, pattern = delimiter, omit_empty = TRUE)
        } else {
            result <- stringi::stri_split_regex(x, pattern = delimiter, omit_empty = TRUE)
        }
        result <- lapply(result, stringi::stri_trim_both)
        names(result) <- names(x)
        result <- unlist(result)
        return(result)
    }
}


