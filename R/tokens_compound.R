#' convert token sequences into compound tokens
#' 
#' Replace multi-token sequences with a multi-word, or "compound" token.  The
#' resulting compound tokens will represent a phrase or multi-word expression, 
#' concatenated with  \code{concatenator} (by default, the "\code{_}" character)
#' to form a single "token".  This ensures that the sequences will be processed
#' subsequently as single tokens, for instance in constructing a \link{dfm}.
#' @param x an input \link{tokens} object
#' @inheritParams sequence2list
#' @param concatenator the concatenation character that will connect the words 
#'   making up the multi-word sequences.  The default \code{_} is highly 
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the Unicode punctuation class [P] will be removed.
#' @inheritParams valuetype
#' @param case_insensitive logical; if \code{TRUE}, ignore case when matching
#' @return a \link{tokens} object in which the token sequences matching the patterns 
#' in \code{sequences} have been replaced by  compound "tokens" joined by the concatenator
#' @export
#' @author Kenneth Benoit (R) and Kohei Watanabe (C++)
#' @examples
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised taxes: an income tax and inheritance taxes.")
#' mytoks <- tokens(mytexts, removePunct = TRUE)
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
#' collocs <- collocations("capital gains taxes are worse than inheritance taxes", size = 2:3)
#' toks <- tokens("The new law included capital gains taxes and inheritance taxes.")
#' tokens_compound(toks, collocs)
tokens_compound <- function(x, sequences,
                    concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                    case_insensitive = TRUE) {
    UseMethod("tokens_compound")
}


#' @rdname tokens_compound
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_compound.tokens <- function(x, sequences,
                   concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                   case_insensitive = TRUE) {
    
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    sequences <- sequence2list(sequences)
    seqs <- as.list(sequences)
    seqs <- seqs[lengths(seqs) > 1] # drop single words
    if (!length(seqs))
        stop("No sequence is provided")
    
    valuetype <- match.arg(valuetype)
    valuetype <- match.arg(valuetype)
    
    names_org <- names(x)
    attrs_org <- attributes(x)
    types <- types(x)
    
    # Convert glob or regex to fixed
    seqs_id <- regex2id(seqs, types, valuetype, case_insensitive)
    if(length(seqs_id) == 0) return(x) # do nothing
    
    # Make new types
    seqs_type <- stringi::stri_c_list(lapply(seqs_id, function(y) types[y]), sep=concatenator)
    
    
    # Assign IDs to new types
    types_id <- match(seqs_type, types)
    types_new <- seqs_type[is.na(types_id)]
    types_id[is.na(types_id)] <- seq(length(types) + 1, by=1, length.out=length(types_new))
    x <- qatd_cpp_tokens_replace(x, seqs_id, types_id)
    
    names(x) <- names_org
    attributes(x) <- attrs_org
    types(x) <- c(types, types_new)
    
    tokens_hashed_recompile(x)
}


#' @noRd
#' @rdname tokens_compound
#' @export
tokens_compound.tokenizedTexts <- function(x, sequences, 
                                           concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                                           case_insensitive = TRUE) {
    as.tokenizedTexts(tokens_compound(as.tokens(x), sequences = sequences, 
                                      concatenator = concatenator, valuetype = valuetype,
                                      case_insensitive = TRUE))
}


