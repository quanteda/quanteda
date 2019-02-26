#' Convert token sequences into compound tokens
#'
#' Replace multi-token sequences with a multi-word, or "compound" token.  The
#' resulting compound tokens will represent a phrase or multi-word expression,
#' concatenated with \code{concatenator} (by default, the "\code{_}" character)
#' to form a single "token".  This ensures that the sequences will be processed
#' subsequently as single tokens, for instance in constructing a \link{dfm}.
#' @param x an input \link{tokens} object
#' @inheritParams pattern
#' @param concatenator the concatenation character that will connect the words
#'   making up the multi-word sequences.  The default \code{_} is recommended
#'   since it will not be removed during normal cleaning and tokenization (while
#'   nearly all other punctuation characters, at least those in the Unicode
#'   punctuation class [P] will be removed).
#' @inheritParams valuetype
#' @param case_insensitive logical; if \code{TRUE}, ignore case when matching.
#'   When \code{pattern} is a \code{collocations}, case-sensitive operation is
#'   significantly faster than case-insensitive operation.
#' @param join logical; if \code{TRUE}, join overlapping compounds into a single
#'   compound; otherwise, form these separately.  See examples.
#' @return A \link{tokens} object in which the token sequences matching
#'   \code{pattern} have been replaced by  compound "tokens" joined by the
#'   concatenator.
#' @note Patterns to be compounded (naturally) consist of multi-word sequences,
#'   and how these are expected in \code{pattern} is very specific.  If the
#'   elements to be compounded are supplied as space-delimited elements of a
#'   character vector, wrap the vector in \code{\link{phrase}}.  If the elements
#'   to be compounded are separate elements of a character vector, supply it as
#'   a list where each list element is the sequence of character elements.
#'   
#'   See the examples below.
#' @export
#' @examples
#' txt <- "The United Kingdom is leaving the European Union."
#' toks <- tokens(txt, remove_punct = TRUE)
#' 
#' # character vector - not compounded
#' tokens_compound(toks, c("United", "Kingdom", "European", "Union"))
#'
#' # elements separated by spaces - not compounded
#' tokens_compound(toks, c("United Kingdom", "European Union"))
#' 
#' # list of characters - is compounded
#' tokens_compound(toks, list(c("United", "Kingdom"), c("European", "Union")))
#'
#' # elements separated by spaces, wrapped in phrase)() - is compounded
#' tokens_compound(toks, phrase(c("United Kingdom", "European Union")))
#' 
#' # supplied as values in a dictionary (same as list) - is compounded
#' # (keys do not matter)
#' tokens_compound(toks, dictionary(list(key1 = "United Kingdom", 
#'                                       key2 = "European Union")))
#' # pattern as dictionaries with glob matches
#' tokens_compound(toks, dictionary(list(key1 = c("U* K*"))), valuetype = "glob")
#' 
#' # supplied as collocations - is compounded
#' colls <- tokens("The new European Union is not the old European Union.") %>%
#'     textstat_collocations(size = 2, min_count = 1, tolower = FALSE)
#' tokens_compound(toks, colls, case_insensitive = FALSE)
#' 
#' # note the differences caused by join = FALSE
#' compounds <- list(c("the", "European"), c("European", "Union"))
#' tokens_compound(toks, pattern = compounds, join = TRUE)
#' tokens_compound(toks, pattern = compounds, join = FALSE)
tokens_compound <- function(x, pattern,
                    concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                    case_insensitive = TRUE, join = TRUE) {
    UseMethod("tokens_compound")
}

#' @export
tokens_compound.default <- function(x, pattern,
                                   concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                                   case_insensitive = TRUE, join = TRUE) {
    stop(friendly_class_undefined_message(class(x), "tokens_compound"))
}

#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_compound.tokens <- function(x, pattern,
                   concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                   case_insensitive = TRUE, join = TRUE) {
    
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    types <- types(x)
    
    seqs_id <- pattern2list(pattern, types, valuetype, case_insensitive, remove_unigram = TRUE)
    if (length(seqs_id) == 0) return(x) # do nothing
    x <- qatd_cpp_tokens_compound(x, seqs_id, types, concatenator, join)
    attributes(x, FALSE) <- attrs
    attr(x, "concatenator") <- concatenator
    return(x)
}
