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
#' @param what unit of segmentation.  Current options are \code{"tokens"} 
#'   (default), \code{"sentences"}, \code{"paragraphs"}, \code{"tags"}, and 
#'   \code{"other"}.  Segmenting on \code{other} allows segmentation of a text 
#'   on any user-defined value, and must be accompanied by the \code{delimiter} 
#'   argument.  Segmenting on \code{tags} performs the same function but 
#'   preserves the tags as a document variable in the segmented corpus.
#' @param delimiter  delimiter defined as a \code{\link{regex}} for 
#'   segmentation. Each type has its own default, except \code{other}, which 
#'   requires a value to be specified.
#' @param valuetype how to interpret the delimiter: \code{fixed} for exact 
#'   matching; \code{"regex"} for regular expressions; or \code{"glob"} for 
#'   "glob"-style wildcard patterns
#' @param perl logical. Should Perl-compatible regular expressions be used?
#' @param x corpus object whose texts will be segmented
#' @param keepdocvars (for corpus objects) if \code{TRUE}, repeat the docvar values for each 
#'   segmented text; if \code{FALSE}, drop the docvars in the segmented corpus. 
#'   Dropping the docvars might be useful in order to conserve space or if these
#'   are not desired for the segmented corpus.
#' @param ... provides additional arguments passed to \code{\link{tokens}}, if 
#'   \code{what = "tokens"} is used
#' @return A corpus of segmented texts.
#' @note Does not currently record document segments if segmenting a multi-text 
#'   corpus into smaller units. For this, use \link{changeunits} instead.
#' @details Tokens are delimited by separators.  For sentences, the delimiter 
#'   can be defined by the user.  The default for sentences includes \code{.}, 
#'   \code{!}, \code{?}, plus \code{;} and \code{:}.
#'   
#'   For paragraphs, the default is two carriage returns, although this could be
#'   changed to a single carriage return by changing the value of 
#'   \code{delimiter} to \code{"\\\n{1}"} which is the R version of the 
#'   \code{\link{regex}} for one newline character.  (You might need this if the
#'   document was created in a word processor, for instance, and the lines were 
#'   wrapped in the window rather than being hard-wrapped with a newline 
#'   character.)
#' @examples
#' testCorpus <- corpus(c("##INTRO This is the introduction. 
#'                        ##DOC1 This is the first document.  
#'                        Second sentence in Doc 1.  
#'                        ##DOC3 Third document starts here.  
#'                        End of third document.",
#'                       "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
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
corpus_segment <- function(x, what = c("tokens", "sentences", "paragraphs", "tags", "other"), 
                           delimiter = ifelse(what=="tokens", " ", 
                                              ifelse(what=="sentences", "[.!?:;]", 
                                                     ifelse(what=="paragraphs", "\\n{2}", 
                                                            ifelse(what=="tags", "##\\w+\\b", 
                                                                   NULL)))),
                           valuetype = c("regex", "fixed", "glob"),
                           perl=FALSE,
                           keepdocvars = TRUE, 
                           ...) {
    if (!is.corpus(x))
        stop("x must be a corpus object")
    
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    # automatically detect and override valuetype
    if (gsub("[*?]|\\w|\\s", "", delimiter) != "" & valuetype != "regex") {
        warning("delimiter looks like it contains a regex", noBreaks. = TRUE)
    } else if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(delimiter, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(delimiter, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    segTxt <- char_segment(texts(x), what, delimiter, perl = perl, valuetype = valuetype, ...)
    names(segTxt) <- paste0(names(segTxt), ".")
    
    newCorpus <- corpus(unlist(segTxt),
                        source = metacorpus(x, "source"),
                        notes = paste0("corpus_segment(", match.call(), ")"))
    
    if (what == "tags") {
        tagIndex <- gregexpr(delimiter, cattxt <- paste0(texts(x), collapse = ""), 
                             perl = perl, fixed = (valuetype == "fixed"))[[1]]
        tags <- character()
        length(tags) <- ndoc(newCorpus)
        for (i in 1:length(tagIndex))
            tags[i] <- substr(cattxt, start = tagIndex[i],
                              stop = tagIndex[i] + attr(tagIndex, "match.length")[i] - 1)
        # remove white space at both ends
        tags <- stringi::stri_trim_both(tags)
        # add tag as a docvar
        docvars(newCorpus, "tag") <- tags
    }
    
    # add repeated versions of remaining docvars
    if (!is.null(docvars(x)) & keepdocvars)
        docvars(newCorpus) <- cbind(docvars(newCorpus), lapply(docvars(x), rep, lengths(segTxt)))
    
    newCorpus
}


#' @rdname corpus_segment
#' @export
#' @examples
#' # same as tokenize()
#' identical(tokenize(data_char_ukimmig2010), char_segment(data_char_ukimmig2010))
#' 
#' # segment into paragraphs
#' char_segment(data_char_ukimmig2010[3:4], "paragraphs")
#' 
#' # segment a text into sentences
#' segmentedChar <- char_segment(data_char_ukimmig2010, "sentences")
#' segmentedChar[2]
char_segment <- function(x, what=c("tokens", "sentences", "paragraphs", "tags", "other"), 
                              delimiter = ifelse(what=="tokens", " ", 
                                                 ifelse(what=="sentences", "[.!?:;]", 
                                                        ifelse(what=="paragraphs", "\\n{2}", 
                                                               ifelse(what=="tags", "##\\w+\\b", 
                                                                      NULL)))),
                              valuetype = c("regex", "fixed", "glob"),
                              perl=FALSE,
                              ...) {
   
    if (!all(is.character(x)))
        stop("x must be of character type")
    
     what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(delimiter, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(delimiter, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (what=="tokens") {
        return(tokenize(x, ...)) 
    } else if (what=="sentences") {
        # warning("consider using tokenize(x, what = \"sentence\") instead.")
        return(lapply(x, segmentSentence, delimiter, perl=perl)) 
    } else if (what=="paragraphs") {
        return(lapply(x, segmentParagraph, delimiter, perl = perl, fixed = (valuetype == "fixed"))) 
    } else if (what=="tags") {
        return(lapply(x, segmentParagraph, delimiter, perl = perl, fixed = (valuetype == "fixed")))         
    } else if (what=="other") {
        if (is.null(delimiter))
            stop("For type other, you must supply a delimiter value.")
        return(lapply(x, segmentParagraph, delimiter, perl = perl, fixed = (valuetype == "fixed"))) 
    }
}

# @rdname segment
# @return \code{segmentSentence} returns a character vector of sentences that
#   have been segmented
# @export
# @examples
# # segment sentences of the UK 2010 immigration sections of manifestos
# segmentSentence(data_char_ukimmig2010[1])[1:5]   # 1st 5 sentences from first (BNP) text
# str(segmentSentence(data_char_ukimmig2010[1]))   # a 132-element char vector
# str(segmentSentence(data_char_ukimmig2010[1:2])) # a 144-element char vector (143+ 12)
# 
segmentSentence <- function(x, delimiter = NULL, perl = FALSE) {
    result <- unlist(tokenize(x, what = "sentence"), use.names = FALSE)
}

# @rdname segment
# @return \code{segmentParagraph} returns a character vector of paragraphs that
#   have been segmented
# @export
# @examples
# # segment paragraphs 
# segmentParagraph(data_char_ukimmig2010[3])[1:2]   # 1st 2 Paragraphs from 3rd (Con) text
# str(segmentParagraph(data_char_ukimmig2010[3]))   # a 12-element char vector
# 
# @export
segmentParagraph <- function(x, delimiter="\\n{2}", perl = FALSE, fixed = FALSE) {
    tmp <- unlist(strsplit(x, delimiter, fixed = fixed, perl = perl))
    tmp[which(tmp != "")]
}

