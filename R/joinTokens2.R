
#' join tokens function
#' 
#' For a set of tokens, given a set of token sequences, join the tokens matching the sequences.
#' @param x tokens or tokenizedTexts object
#' @param ... additional arguments passed to other methods
#' @export
joinTokens2 <- function(x, ...) {
    UseMethod("joinTokens2")
}

#' @rdname joinTokens
#' @export
joinTokens2.tokenizedTexts <- function(x, ...) {
    as.tokenizedTexts(joinTokens2(as.tokens(x), ...))
}


#' @rdname joinTokens
#' @param sequences features to concatenate, a list of characters
#' @param concatenator character used for joining tokens
#' @param valuetype how to interpret sequences: \code{fixed} for words as
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for
#'   "glob"-style wildcard
#' @param case_insensitive if \code{TRUE}, ignore case when matching
#' @param verbose display progress
#' @author Kohei Watanabe and Kenneth Benoit
#' @examples
#' # simple example
#' txt <- c("a b c d e f g", "A B C D E F G", "A b C d E f G", 
#'          "aaa bbb ccc ddd eee fff ggg", "a_b b_c c_d d_e e_f f_g") 
#' toks <- tokens(txt)
#' seqs <- tokens(c("a b", "C D", "aa* bb*", "eEE FFf", "d_e e_f", "z z"), 
#'                hash = FALSE, what = "fastestword")
#' joinTokens(toks, seqs, valuetype = "glob", case_insensitive = TRUE)
#' joinTokens2(toks, seqs, valuetype = "glob", case_insensitive = TRUE)
#' 
#' microbenchmark::microbenchmark(
#' joinTokens(toks, seqs, valuetype = "glob", case_insensitive = TRUE),
#' joinTokens2(toks, seqs, valuetype = "glob", case_insensitive = TRUE))


#' joinTokens(toks, seqs, valuetype = "glob", case_insensitive = FALSE)
#' 
#' seqs <- tokens(c("a b", "C D", "aa bb", "eEE FFf", "d_e e_f"), 
#'                hash = FALSE, what = "fastestword")
#' joinTokens(toks, seqs, valuetype = "fixed", case_insensitive = TRUE)
#' joinTokens(toks, seqs, valuetype = "fixed", case_insensitive = FALSE)
#'                
#' @export
joinTokens2.tokens <- function(x, sequences, concatenator = "_", 
                              valuetype = c("glob", "fixed", "regex"), 
                              verbose = FALSE, case_insensitive = TRUE, ...) {
    
    valuetype <- match.arg(valuetype)
    
    # Initialize
    seqs <- as.list(sequences)
    seqs <- seqs[lengths(seqs) > 1] # drop single words
    types <- types(x)
    
    # Convert to regular expressions, then to fixed
    if (valuetype %in% c("glob"))
        seqs_regex <- lapply(seqs, glob2rx)
    if (valuetype %in% c("glob", "regex")) {
        # Generates all possible patterns of keys
        seqs_fixed <- regex2fixed(seqs_regex, types, valuetype, case_insensitive)
    } else {
        seqs_fixed <- seqs
    }
    if(verbose) message(sprintf('Join %d pairs of tokens', length(seqs_fixed)))
    if(length(seqs_fixed) == 0) return(x) # do nothing
    
    seqs_id <- lapply(seqs_fixed, fmatch, table=types)
    seqs_type <- sapply(seqs_fixed, paste0, collapse = concatenator)
    ids <- fmatch(seqs_type, types)
    res <- qatd_cpp_replace_int_list(x, seqs_id, ids, length(types) + 1)

    # Select and types to add
    ids_new <- res$id_new
    names(ids_new) <- seqs_type
    ids_new <- sort(ids_new)
    types <- c(types, names(ids_new[length(types) < ids_new]))
    
    tokens <- res$text
    attributes(tokens) <- attributes(x)
    types(tokens) <- types
    
    return(tokens)
    
}

