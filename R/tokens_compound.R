#' Convert token sequences into compound tokens
#' 
#' Replace multi-token sequences with a multi-word, or "compound" token.  The
#' resulting compound tokens will represent a phrase or multi-word expression, 
#' concatenated with  \code{concatenator} (by default, the "\code{_}" character)
#' to form a single "token".  This ensures that the sequences will be processed
#' subsequently as single tokens, for instance in constructing a \link{dfm}.
#' @param x an input \link{tokens} object
#' @inheritParams pattern
#' @param concatenator the concatenation character that will connect the words 
#'   making up the multi-word sequences.  The default \code{_} is  
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the Unicode punctuation class [P] will be removed).
#' @inheritParams valuetype
#' @param case_insensitive logical; if \code{TRUE}, ignore case when matching
#' @param join logical; if \code{TRUE}, join overlapping compounds
#' @return a \link{tokens} object in which the token sequences matching \code{pattern}
#' have been replaced by  compound "tokens" joined by the concatenator
#' @export
#' @author Kenneth Benoit and Kohei Watanabe
#' @examples
#' txt <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised taxes: an income tax and inheritance taxes.")
#' toks1 <- tokens(txt, remove_punct = TRUE)
#' 
#' # for lists of sequence elements
#' myseqs <- list(c("tax"), c("income", "tax"), c("capital", "gains", "tax"), c("inheritance", "tax"))
#' (toks2 <- tokens_compound(toks1, pattern = myseqs))
#' dfm(toks2)
#' 
#' # when used as a dictionary for dfm creation
#' dict1 <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax*")))
#' (toks3 <- tokens_compound(toks1, pattern = dict1))
#' 
#' # to pick up "taxes" in the second text, set valuetype = "regex"
#' (toks4 <- tokens_compound(toks1, pattern = dict1, valuetype = "regex"))
#' 
#' # dictionaries w/glob matches
#' dict2 <- dictionary(list(negative = c("bad* word*", "negative", "awful text"),
#'                           positive = c("good stuff", "like? th??")))
#' toks5 <- tokens(c(txt1 = "I liked this, when we can use bad words, in awful text.",
#'                  txt2 = "Some damn good stuff, like the text, she likes that too."))
#' tokens_compound(toks5, pattern = dict2)
#'
#' # with collocations
#' tstat <- textstat_collocations(tokens("capital gains taxes are worse than inheritance taxes"), 
#'                               size = 2, min_count = 1)
#' toks6 <- tokens("The new law included capital gains taxes and inheritance taxes.")
#' tokens_compound(toks6, pattern = tstat)
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
