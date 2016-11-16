
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
#' 
#' toks <- tokens(inaugCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' seqs_glob <- list(c('foreign', 'polic*'), c('United', 'States'))
#' seqs_regex <- list(c('^foreign', '^polic(ie|y)'), c('^United', '^States'))
#' toks2 <- joinTokens(toks, seqs_token, "_", 'fixed')
#' toks2 <- joinTokens(toks, seqs_glob, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_regex, "_", 'regex')
#' kwic(toks2, 'foreign_policy', window = 1) # joined
#' kwic(toks2, c('foreign', 'policy'), window = 1) # not joined
#' kwic(toks2, 'United_States', window = 1) # joined
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

