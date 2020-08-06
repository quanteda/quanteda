#' Segment texts on a pattern match
#'
#' Segment corpus text(s) or a character vector, splitting
#' on a pattern match.  This is useful for breaking the texts into smaller
#' documents based on a regular pattern (such as a speaker identifier in a
#' transcript) or a user-supplied annotation.
#'
#' For segmentation into syntactic units defined by the locale (such as
#' sentences), use [corpus_reshape()] instead.  In cases where more
#' fine-grained segmentation is needed, such as that based on commas or
#' semi-colons (phrase delimiters within a sentence),
#' [corpus_segment()] offers greater user control than
#' [corpus_reshape()].
#' @param x character or [corpus] object whose texts will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param extract_pattern extracts matched patterns from the texts and save in docvars if
#'   `TRUE`
#' @param pattern_position either `"before"` or `"after"`, depending
#'   on whether the pattern precedes the text (as with a user-supplied tag, such
#'   as `##INTRO` in the examples below) or follows the text (as with
#'   punctuation delimiters)
#' @param use_docvars if `TRUE`, repeat the docvar values for each
#'   segmented text; if `FALSE`, drop the docvars in the segmented corpus.
#'   Dropping the docvars might be useful in order to conserve space or if these
#'   are not desired for the segmented corpus.
#' @return `corpus_segment` returns a corpus of segmented texts
#' @keywords corpus
#' @section Boundaries and segmentation explained: The `pattern` acts as a
#'   boundary delimiter that defines the segmentation points for splitting a
#'   text into new "document" units.  Boundaries are always defined as the
#'   pattern matches, plus the end and beginnings of each document.  The new
#'   "documents" that are created following the segmentation will then be the
#'   texts found between boundaries.
#'
#'   The pattern itself will be saved as a new document variable named
#'   `pattern`.  This is most useful when segmenting a text according to
#'   tags such as names in a transcript, section titles, or user-supplied
#'   annotations.  If the beginning of the file precedes a pattern match, then
#'   the extracted text will have a `NA` for the extracted `pattern`
#'   document variable (or when `pattern_position = "after"`, this will be
#'   true for the text split between the last pattern match and the end of the
#'   document).
#'
#'   To extract syntactically defined sub-document units such as sentences and
#'   paragraphs, use [corpus_reshape()] instead.
#' @section Using patterns: One of the most common uses for
#'   `corpus_segment` is to partition a corpus into sub-documents using
#'   tags.  The default pattern value is designed for a user-annotated tag that
#'   is a term beginning with double "hash" signs, followed by a whitespace, for
#'   instance as `##INTRODUCTION The text`.
#'
#'   Glob and fixed pattern types use a whitespace character to signal the end
#'   of the pattern.
#'
#'   For more advanced pattern matches that could include whitespace or
#'   newlines, a regex pattern type can be used, for instance a text such as
#'
#'   `Mr. Smith: Text` \cr `Mrs. Jones: More text`
#'
#'   could have as `pattern = "\\b[A-Z].+\\.\\s[A-Z][a-z]+:"`, which
#'   would catch the title, the name, and the colon.
#'
#'   For custom boundary delimitation using punctuation characters that come
#'   come at the end of a clause or sentence (such as `,` and`.`,
#'   these can be specified manually and `pattern_position` set to
#'   `"after"`. To keep the punctuation characters in the text (as with
#'   sentence segmentation), set `extract_pattern = FALSE`.  (With most tag
#'   applications, users will want to remove the patterns from the text, as they
#'   are annotations rather than parts of the text itself.)
#' @seealso [corpus_reshape()], for segmenting texts into pre-defined
#'   syntactic units such as sentences, paragraphs, or fixed-length chunks
#' @examples
#' ## segmenting a corpus
#'
#' # segmenting a corpus using tags
#' corp1 <- corpus(c("##INTRO This is the introduction.
#'                   ##DOC1 This is the first document.  Second sentence in Doc 1.
#'                   ##DOC3 Third document starts here.  End of third document.",
#'                  "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#' corpseg1 <- corpus_segment(corp1, pattern = "##*")
#' cbind(texts(corpseg1), docvars(corpseg1))
#'
#' # segmenting a transcript based on speaker identifiers
#' corp2 <- corpus("Mr. Smith: Text.\nMrs. Jones: More text.\nMr. Smith: I'm speaking, again.")
#' corpseg2 <- corpus_segment(corp2, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:",
#'                            valuetype = "regex")
#' cbind(texts(corpseg2), docvars(corpseg2))
#'
#' # segmenting a corpus using crude end-of-sentence segmentation
#' corpseg3 <- corpus_segment(corp1, pattern = ".", valuetype = "fixed",
#'                            pattern_position = "after", extract_pattern = FALSE)
#' cbind(texts(corpseg3), docvars(corpseg3))
#' 
#' @importFrom stringi stri_trim_both stri_replace_all_fixed stri_locate_last_fixed 
#'   stri_locate_first_fixed stri_sub
#' @export
corpus_segment <- function(x, pattern = "##*",
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           extract_pattern = TRUE,
                           pattern_position = c("before", "after"),
                           use_docvars = TRUE) {
    UseMethod("corpus_segment")
}

#' @export
corpus_segment.default <- function(x, pattern = "##*",
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE,
                                  extract_pattern = TRUE,
                                  pattern_position = c("before", "after"),
                                  use_docvars = TRUE) {
    stop(friendly_class_undefined_message(class(x), "corpus_segment"))
}

#' @export
corpus_segment.corpus <- function(x, pattern = "##*",
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE,
                                  extract_pattern = TRUE,
                                  pattern_position = c("before", "after"),
                                  use_docvars = TRUE) {

    x <- as.corpus(x)
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    attrs <- attributes(x)

    temp <- segment_texts(texts(x), pattern, valuetype, case_insensitive,
                          extract_pattern, pattern_position)

    if (use_docvars) {
        docvars <- select_docvars(attrs[["docvars"]], user = TRUE, system = TRUE)
    } else {
        docvars <- select_docvars(attrs[["docvars"]], user = FALSE, system = TRUE)
    }
    docvars <- reshape_docvars(docvars, temp[["docnum"]])
    if (extract_pattern)
        docvars[["pattern"]] <- temp[["pattern"]]

    build_corpus(
        temp[["text"]],
        unit = "segments",
        docvars = docvars,
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
}


#' @rdname corpus_segment
#' @param remove_pattern removes matched patterns from the texts if `TRUE`
#' @export
#' @examples
#' ## segmenting a character vector
#'
#' # segment into paragraphs and removing the "- " bullet points
#' cat(data_char_ukimmig2010[4])
#' char_segment(data_char_ukimmig2010[4],
#'              pattern = "\\n\\n(-\\s){0,1}", valuetype = "regex",
#'              remove_pattern = TRUE)
#'
#' # segment a text into clauses
#' txt <- c(d1 = "This, is a sentence?  You: come here.", d2 = "Yes, yes okay.")
#' char_segment(txt, pattern = "\\p{P}", valuetype = "regex",
#'              pattern_position = "after", remove_pattern = FALSE)
#' @keywords character
#' @return `char_segment` returns a character vector of segmented texts
char_segment <- function(x, pattern = "##*",
                         valuetype = c("glob", "regex", "fixed"),
                         case_insensitive = TRUE,
                         remove_pattern = TRUE,
                         pattern_position = c("before", "after")) {
    UseMethod("char_segment")
}

#' @export
char_segment.default <- function(x, pattern = "##*",
                                 valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE,
                                 remove_pattern = TRUE,
                                 pattern_position = c("before", "after")) {
    stop(friendly_class_undefined_message(class(x), "char_segment"))
}

#' @export
#' @importFrom stringi stri_locate_first_regex stri_trim_both stri_locate_last_regex
char_segment.character <- function(x, pattern = "##*",
                                   valuetype = c("glob", "regex", "fixed"),
                                   case_insensitive = TRUE,
                                   remove_pattern = TRUE,
                                   pattern_position = c("before", "after")) {

    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    temp <- corpus_segment(corpus(x, names(x), unique_docnames = FALSE),
                           pattern, valuetype, case_insensitive,
                           remove_pattern, pattern_position)
    result <- texts(temp)
    if (is.null(names(x)))
        names(result) <- NULL
    return(result)
}

# internal functions ----------

# internal function for char_segment and corpus_segment
segment_texts <- function(x, pattern = NULL, valuetype = "regex",
                          case_insensitive = TRUE,
                          extract_pattern = FALSE, pattern_position = "after",
                          omit_empty = TRUE, what = "other", ...) {

    # normalize EOL
    x <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh

    # use preset regex pattern
    if (what == "paragraphs") {
        pattern <- "\\n\\n"
        valuetype <- "regex"
    }

    if (is.null(pattern)) {
        if (what == "sentences")
            x <- tokenize_sentence(x, ...)
    } else {

        if (valuetype == "glob") {
            # treat as fixed if no glob character is detected
            if (!any(stri_detect_charclass(pattern, c("[*?]")))) {
                valuetype <- "fixed"
            } else {
                pattern <- escape_regex(pattern)
                pattern <- stri_replace_all_fixed(pattern, '*', '(\\S*)')
                pattern <- stri_replace_all_fixed(pattern, '?', '(\\S)')
                valuetype <- "regex"
            }
        }
        
        x <- stri_trim_both(x)
        
        if (valuetype == "fixed") {
            if (pattern_position == "after") {
                x <- stri_replace_all_fixed(x, pattern, stri_c(pattern, "\uE000"),
                                            case_insensitive = case_insensitive,
                                            vectorize_all = FALSE)
            } else {
                x <- stri_replace_all_fixed(x, pattern, stri_c("\uE000", pattern),
                                            case_insensitive = case_insensitive,
                                            vectorize_all = FALSE)
            }
        } else {
            if (pattern_position == "after") {
                x <- stri_replace_all_regex(x, pattern, "$0\uE000",
                                            case_insensitive = case_insensitive,
                                            vectorize_all = FALSE)
            } else {
                x <- stri_replace_all_regex(x, pattern, "\uE000$0",
                                            case_insensitive = case_insensitive,
                                            vectorize_all = FALSE)
            }
        }
        # keep empty document for corpus_reshape
        x <- stri_split_fixed(x, pattern = "\uE000", omit_empty = omit_empty)
    }
    n <- lengths(x)
    x <- unlist(x, use.names = FALSE)

    if (extract_pattern) {
        pos <- matrix(rep(NA, length(x) * 2), nrow = length(x))
        for (pat in pattern) {
            if (valuetype == "fixed") {
                if (pattern_position == "after") {
                    temp <- stri_locate_last_fixed(x, pat, case_insensitive = case_insensitive)
                } else {
                    temp <- stri_locate_first_fixed(x, pat, case_insensitive = case_insensitive)
                }
            } else {
                if (pattern_position == "after") {
                    temp <- stri_locate_last_regex(x, pat, case_insensitive = case_insensitive)
                } else {
                    temp <- stri_locate_first_regex(x, pat, case_insensitive = case_insensitive)
                }
            }
            pos[, 1] <- pmin(pos[, 1], temp[, 1], na.rm = TRUE)
            pos[, 2] <- pmax(pos[, 2], temp[, 2], na.rm = TRUE)
        }
        if (pattern_position == "after") {
            txt <- stri_sub(x, 1L, pos[, 1] - 1L)
        } else {
            txt <- stri_sub(x, pos[, 2] + 1L, -1L)
        }
        result <- data.frame(text = stri_trim_both(txt),
                             pattern = stri_sub(x, pos[, 1], pos[, 2]),
                             docnum = rep(seq_along(n), n),
                             stringsAsFactors = FALSE)
    } else {
        result <- data.frame(text = stri_trim_both(x),
                             docnum = rep(seq_along(n), n),
                             stringsAsFactors = FALSE)
    }
    result$docnum <- rep(seq_along(n), n)
    if (extract_pattern) {
        result <- result[!is.na(result$pattern), ]
    } else {
        if (omit_empty)
            result <- result[!is.na(result$text), ]
    }
    return(as.list(result))
}
