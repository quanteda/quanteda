#' convert token sequences into compound tokens
#' 
#' Replace multi-token sequences with a multi-word, or "compound" token.  The
#' resulting compound tokens will represent a phrase or multi-word expression, 
#' concatenated with  \code{concatenator} (by default, the "\code{_}" character)
#' to form a single "token".  This ensures that the sequences will be processed
#' subsequently as single tokens, for instance in constructing a \link{dfm}.
#' @param x an input \link{tokens} object
#' @param sequences the input sequence, one of: \itemize{ \item{character vector,
#'   }{whose elements will be split on whitespace;} \item{list of characters,
#'   }{consisting of a list of token patterns, separated by white space}; 
#'   \item{\link{tokens} object;} \item{\link{dictionary} object}{;} 
#'   \item{\link{collocations} object.}{} }
#' @param concatenator the concatenation character that will connect the words 
#'   making up the multi-word sequences.  The default \code{_} is highly 
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the Unicode punctuation class [P] will be removed).
#' @inheritParams valuetype
#' @param case_insensitive logical; if \code{TRUE}, ignore case when matching
#' @param join logical; if \code{TRUE}, join overlapped compounds
#' @return a \link{tokens} object in which the token sequences matching the patterns 
#' in \code{sequences} have been replaced by  compound "tokens" joined by the concatenator
#' @export
#' @author Kenneth Benoit (R) and Kohei Watanabe (C++)
#' @examples
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised taxes: an income tax and inheritance taxes.")
#' mytoks <- tokens(mytexts, remove_punct = TRUE)
#' 
#' # for lists of sequence elements
#' myseqs <- list(c("tax"), c("income", "tax"), c("capital", "gains", "tax"), c("inheritance", "tax"))
#' (cw <- tokens_compound(mytoks, myseqs))
#' dfm(cw)
#' 
#' # when used as a dictionary for dfm creation
#' mydict <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax")))
#' (cw2 <- tokens_compound(mytoks, mydict))
#' 
#' # to pick up "taxes" in the second text, set valuetype = "regex"
#' (cw3 <- tokens_compound(mytoks, mydict, valuetype = "regex"))
#' 
#' # dictionaries w/glob matches
#' myDict <- dictionary(list(negative = c("bad* word*", "negative", "awful text"),
#'                           positive = c("good stuff", "like? th??")))
#' toks <- tokens(c(txt1 = "I liked this, when we can use bad words, in awful text.",
#'                  txt2 = "Some damn good stuff, like the text, she likes that too."))
#' tokens_compound(toks, myDict)
#'
#' # with collocations
#' #cols <- textstat_collocations("capital gains taxes are worse than inheritance taxes", 
#' #                              size = 2, min_count = 1)
#' #toks <- tokens("The new law included capital gains taxes and inheritance taxes.")
#' #tokens_compound(toks, cols)
tokens_compound <- function(x, sequences,
                    concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                    case_insensitive = TRUE, join = FALSE) {
    UseMethod("tokens_compound")
}


#' @rdname tokens_compound
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_compound.tokens <- function(x, sequences,
                   concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                   case_insensitive = TRUE, join = FALSE) {
    
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    valuetype <- match.arg(valuetype)

    names_org <- names(x)
    attrs_org <- attributes(x)
    types <- types(x)
    
    if (is.sequences(sequences) || is.collocations(sequences)) {
        if (identical(attr(sequences, 'types'), types)) {
            #cat("Skip regex2id\n")
            seqs_ids <- attr(sequences, 'tokens')
        } else { 
            #cat("Use regex2id\n")
            seqs <- features2list(sequences$collocation)
            seqs_ids <- regex2id(seqs, types, valuetype, case_insensitive)
        }
    } else {
        #cat("Use regex2id\n")
        seqs <- features2list(sequences)
        seqs <- seqs[lengths(seqs) > 1] # drop single words
        seqs_ids <- regex2id(seqs, types, valuetype, case_insensitive)
    }
    if(length(seqs_ids) == 0) return(x) # do nothing
    x <- qatd_cpp_tokens_compound(x, seqs_ids, types, concatenator, join)
    attributes(x, FALSE) <- attrs_org
    attr(x, "concatenator") <- concatenator
    return(x)
}


#' @noRd
#' @rdname tokens_compound
#' @export
tokens_compound.tokenizedTexts <- function(x, sequences, 
                                           concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                                           case_insensitive = TRUE, join = FALSE) {
    as.tokenizedTexts(tokens_compound(as.tokens(x), sequences = sequences, 
                                      concatenator = concatenator, valuetype = valuetype,
                                      case_insensitive = TRUE, join = join))
}


