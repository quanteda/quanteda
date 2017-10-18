
#' replace types in tokens object
#'
#' This funciton substitutes token types based on vectorized one-to-one matching.
#' Since this function is created for lemmatization or user-defined stemming, it
#' does not support multi-word features, or glob and regex patterns. Please use
#' \code{tokens_lookup} with \code{exclusive = FALSE} for substitutions of more
#' complex patterns.
#' @param x \link{tokens} object whose token elements will be replaced
#' @param from a vector of types to be substituted
#' @param to a vector of types to substitute
#' @param case_insensitive ignore case when matching, if \code{TRUE}
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

tokens_replace <- function(x, from, to, case_insensitive = TRUE, 
                           verbose = quanteda_options("verbose")) {
    UseMethod("tokens_replace")
}

#' @rdname tokens_replace
#' @noRd
#' @export
tokens_replace.tokens <- function(x, from, to, case_insensitive = TRUE, 
                                  verbose = quanteda_options("verbose")) {
    
    if (length(from) != length(to))
        stop("Lengths of 'from' and 'to' must be the same")
    if (!length(from) || !length(to))
        return(x)
    
    type <- attr(x, 'types')
    if (case_insensitive) {
        type_new <- to[match(stri_trans_tolower(type), stri_trans_tolower(from))]
    } else {
        type_new <- to[match(type, from)]
    }
    type_new <- ifelse(is.na(type_new), type, type_new)
    attr(x, 'types') <- type_new
    tokens_recompile(x)
}
