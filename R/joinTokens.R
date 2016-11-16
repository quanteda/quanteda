#' join tokens function
#' 
#' For a set of tokens, given a set of token sequences, join the tokens matching the sequences.
#' @param x tokens or tokenizedTexts object
#' @param ... additional arguments passed to other methods
#' @export
joinTokens <- function(x, ...) {
    UseMethod("joinTokens")
}

#' @rdname joinTokens
#' @export
joinTokens.tokenizedTexts <- function(x, ...) {
    as.tokenizedTexts(joinTokens(as.tokens(x), ...))
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
#' toks <- tokens(inaugCorpus, removePunct = TRUE)
#' seqs <- list(c('foreign', 'polic*'), c('United', 'States'))
#' kwic(toks, 'fUnited_States', window = 1) # no exisit
#' kwic(toks, 'foreign_policy', window = 1) # no exisit
#' toks <- joinTokens(toks, seqs, "_", 'glob')
#' kwic(toks, 'United_States', window = 1)
#' kwic(toks, 'foreign_policy', window = 1)
#' kwic(toks, c('foreign', 'policy'), window = 1) # no longer exisit
#' kwic(toks, c('united', 'states'), window = 1) # no longer exisit
#'                
#' @export
joinTokens.tokens <- function(x, sequences, concatenator = "_", 
                              valuetype = c("glob", "fixed", "regex"), 
                              verbose = FALSE, case_insensitive = TRUE, ...) {
    
    valuetype <- match.arg(valuetype)
    
    types <- types(x)
    # if (verbose) message("Indexing tokens...")
    # index <- dfm(x, verbose = FALSE, toLower = FALSE) # index is always case-sensitive
    # index_binary <- as(index, 'nMatrix')
    
    # Convert to regular expressions, then to fixed
    if (valuetype %in% c("glob"))
        sequences <- lapply(sequences, glob2rx)
    if (valuetype %in% c("glob", "regex") | case_insensitive) {
        # Generates all possible patterns of sequences
        seqs_token <- regex2fixed(sequences, types, valuetype, case_insensitive)
    } else {
        seqs_token <- sequences
    }
    
    if (verbose) message(sprintf('Join %d pairs of tokens', length(seqs_token)))
    
    # Check if joined tokens are in vocabulary
    types_new <- sapply(seqs_token, paste0, collapse = concatenator)
    ids_exist <- match(types_new, types)
    id_new <- length(types) + 1
    n_seqs <- length(seqs_token)
    if (n_seqs == 0) return(x)
    for (i in 1:n_seqs) {
        seq_token <- seqs_token[[i]]
        if (length(seq_token) < 2) next
        if (is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) 
            stop('Invalid token sequence\n');
        if (!all(seq_token %in% types)) {
            if (verbose) 
                message(sprintf('%d/%d "%s" is not found', i, n_seqs, paste(seq_token, collapse=' ')))
        } else {
            # flag <- Matrix::rowSums(index_binary[,seq_token, drop = FALSE]) == length(seq_token)
            # if (verbose) 
            #     message(sprintf('%d/%d "%s" is found in %d texts', i, n_seqs, 
            #                     paste(seq_token, collapse=' '), sum(flag)))
            
            # Use exisitng ID
            if (is.na(ids_exist[i])){
                id <- id_new
            } else {
                id <- ids_exist[i]
            }
            if (verbose) message(sprintf(' Use %d for %s ', id, types_new[i]))
            x <- qatd_cpp_replace_hash_list(x, rep(TRUE, length(x)), match(seq_token, types), id)
            
            # Add new ID to types only if used
            if (is.na(ids_exist[i]) & id %in% unlist(x, use.names = FALSE)) {
                if (verbose) message(sprintf(' Add %d for %s', id, types_new[i]))
                types <- c(types, types_new[i])
                id_new <- id_new + 1
            }
        }
    }
    types(x) <- types
    return(x)
}
