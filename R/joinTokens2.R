
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
    seqs <- as.list(sequences)
    
    # Initialize
    tokens <- qatd_cpp_deepcopy(x) # create empty tokens object
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
    if(length(seqs_fixed) == 0) next
    seqs_id <- lapply(seqs_fixed, function(x) fmatch(x, types))
    seqs_type <- sapply(seqs_fixed, paste0, collapse = concatenator)
    res <- qatd_cpp_replace_int_list(x, seqs_id, fmatch(seqs_type, types), length(types) + 1)
    tokens <- res$text
    types <- c(types, seqs_type[res$id_new > 0])

    attributes(tokens) <- attributes(x)
    types(tokens) <- types
    
    return(tokens)
    
}

