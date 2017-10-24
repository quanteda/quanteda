
#' replace types in tokens object
#'
#' This funciton substitutes token types based on vectorized one-to-one matching.
#' Since this function is created for lemmatization or user-defined stemming, it
#' does not support multi-word features, or glob and regex patterns. Please use
#' \code{tokens_lookup} with \code{exclusive = FALSE} for substitutions of more
#' complex patterns.
#' @param x \link{tokens} object whose token elements will be replaced
#' @inheritParams pattern
#' @param replacement a vector of types to substitute
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param verbose print status messages if \code{TRUE}
#' @export
#' @examples
#' toks <- tokens(data_corpus_irishbudget2010)
#'
#' # lemmatization
#' infle <- c('foci', 'focus', 'focused', 'focuses', 'focusing', 'focussed', 'focusses', 'focussing')
#' lemma <- rep('focus', length(infle))
#' toks2 <- tokens_replace(toks, infle, lemma)
#' kwic(toks2, 'focus*')
#'
#' # stemming
#' type <- types(toks)
#' stem <- char_wordstem(type, 'porter')
#' toks3 <- tokens_replace(toks, type, stem, case_insensitive = FALSE)
#' identical(toks3, tokens_wordstem(toks, 'porter'))

tokens_replace <- function(x, pattern, replacement = NULL, case_insensitive = TRUE, 
                           verbose = quanteda_options("verbose")) {
    UseMethod("tokens_replace")
}

#' @rdname tokens_replace
#' @noRd
#' @export
tokens_replace.tokens <- function(x, pattern, replacement = NULL, case_insensitive = TRUE, 
                                  verbose = quanteda_options("verbose")) {
    
    if (is.dictionary(pattern)) {
        if (!is.null(replacement))
            stop("'replacement' must be NULL when 'pattern' is a dictionary")
        pattern <- flatten_dictionary(dict, levels = 1)
        replacement <- rep(names(pattern), lengths(pattern))
        pattern <- unlist(pattern, use.names = FALSE)
    }
    if (length(pattern) != length(replacement))
        stop("Lengths of 'pattern' and 'replacement' must be the same")
    if (!is.character(pattern) || !is.character(replacement))
        stop("'pattern' and 'replacement' must be characters")
    if (!length(pattern) || !length(replacement))
        return(x)
    
    type <- attr(x, 'types')
    if (case_insensitive) {
        type_new <- replacement[match(stri_trans_tolower(type), stri_trans_tolower(pattern))]
    } else {
        type_new <- replacement[match(type, pattern)]
    }
    type_new <- ifelse(is.na(type_new), type, type_new)
    attr(x, 'types') <- type_new
    tokens_recompile(x)
}
