#' join tokens function
#'
#' Needs some more explanation
#' @param x some object
#' @param sequences list of vector of features to concatenate
#' @param concatenator character used for joining tokens
#' @param valuetype how to interpret sequences: \code{fixed} for words as
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for
#'   "glob"-style wildcard
#' @param case_insensitive if \code{TRUE}, ignore case when matching
#' @param verbose display progress
#' @author Kohei Watanabe and Kenneth Benoit
#' @examples
#' # simple example
#' txt <- c("The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
#'          "The Supreme Court of the United States is seldom in a united state.",
#'          "It's Arsenal versus Manchester United, states the announcer.", 
#'          "luv the united states XXOO :-)")
#' toks <- tokenize(txt)
#' phrasesFixed <- tokenize(c("United States", "Supreme Court", "Atlantic Ocean", "Pacific Ocean"))
#' joinTokens(toks, phrasesFixed, valuetype = "fixed", case_insensitive = FALSE, verbose=TRUE)
#' joinTokens(toks, phrasesFixed, valuetype = "fixed", case_insensitive = TRUE, verbose=TRUE)
#' ## NOT WORKING
#' joinTokens(toks, phrasesFixed, valuetype = "regex", verbose = TRUE)
#' 
#' toks <- tokenize("Simon sez the multi word expression plural is multi word expressions, Simon sez.")
#' phrases <- tokenize(c("multi word expression", "multi word expressions", "Simon sez"))
#' joinTokens(toks, phrases, valuetype = "fixed", verbose = TRUE)
#' 
#' # with the inaugural corpus
#' toks <- tokenize(inaugCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' seqs_glob <- list(c('foreign', 'polic*'), c('United', 'States'))
#' seqs_regex <- list(c('^foreign', '^polic(ie|y)'), c('^United', '^States'))
#' toks2 <- joinTokens(toks, seqs_token, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_glob, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_regex, "_", 'regex')
#' kwic(toks2, 'foreign_policy', window = 1) # joined
#' kwic(toks2, c('foreign', 'policy'), window = 1) # not joined
#' kwic(toks2, 'United_States', window = 1) # joined
#' @export
joinTokens <- function(x, sequences, concatenator = "_", valuetype = c("glob", "fixed", "regex"), 
                       verbose = FALSE, case_insensitive = TRUE) {
    valuetype <- match.arg(valuetype)
    
    if (verbose) cat("Indexing tokens...\n")
    index <- dfm(x, verbose = FALSE) # index is always case-sensitive
    index_binary <- as(index, 'nMatrix')
    types <- colnames(index_binary)
    
    # Convert to regular expressions, then to fixed
    if (valuetype %in% c("glob"))
        sequences <- lapply(sequences, glob2rx)
    if (valuetype %in% c("glob", "regex") | case_insensitive) {
        # Generates all possible patterns of sequences
        seqs_token <- grid_sequence(x, sequences, types, valuetype, case_insensitive)
    } else {
        seqs_token <- sequences
    }
    #print(str(seqs_token))
    n_seqs <- length(seqs_token)
    if (n_seqs == 0) return(x)
    y <- deepcopy(x) # copy x to y to prevent changes in x
    for (i in 1:n_seqs) {
        seq_token <- seqs_token[[i]]
        if (length(seq_token) < 2) next
        if (is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
        if (!all(seq_token %in% types)) {
            if(verbose) cat(sprintf('%d/%d "%s" is not found\n', i, n_seqs, paste(seq_token, collapse=' ')))
        } else {
            flag <- Matrix::rowSums(index_binary[,seq_token, drop = FALSE]) == length(seq_token)
            if (verbose) cat(sprintf('%d/%d "%s" is found in %d texts\n', i, n_seqs, paste(seq_token, collapse=' '), sum(flag)))
            join_tokens_cppl(y, flag, seq_token, concatenator) # pass y as reference
        }
    }
    removeFeatures(y, "")
}

grid_sequence <- function(tokens, seqs_pat, types, valuetype, case_insensitive = FALSE) {
    
    seqs_token <- list()
    for (seq_pat in seqs_pat) {
        if(valuetype == 'fixed'){
          seq_match <- lapply(seq_pat, function(x, y) y[toLower(y) %in% toLower(x)], types)
        }else{
          seq_match <- lapply(seq_pat, function(x, y) y[stringi::stri_detect_regex(y, x, case_insensitive = case_insensitive)], types)
        }
        #print(seq_match)
        if (length(unlist(seq_pat)) != length(seq_match)) next
        match_comb <- do.call(expand.grid, c(seq_match, stringsAsFactors = FALSE)) # produce all possible combinations
        #print(match_comb)
        seqs_token <- c(seqs_token, split_df_cpp(t(match_comb)))
    }
    seqs_token
}

## older function called from selectFeatures.R
regexToFixed <- function(tokens, patterns, case_insensitive = FALSE, types = NULL) {
    
    # get unique token types
    if (is.null(types)) types <- unique(unlist(tokens))
    
    seqs_token <- list()
    for (seq_regex in patterns) {
        match <- lapply(seq_regex, function(x, y) y[stringi::stri_detect_regex(y, x, case_insensitive = case_insensitive)], types)
        if (length(unlist(seq_regex)) != length(match)) next
        match_comb <- do.call(expand.grid, c(match, stringsAsFactors = FALSE)) # produce all possible combinations
        seqs_token <- c(seqs_token, split_df_cpp(t(match_comb)))
    }
    seqs_token
}


