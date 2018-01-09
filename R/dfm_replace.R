#' Replace features in dfm
#'
#' Substitute features based on vectorized one-to-one matching for lemmatization
#' or user-defined stemming.
#' @param x \link{dfm} whose features will be replaced
#' @param pattern a character vector or \link{dictionary}.  See \link{pattern}
#'   for more details.
#' @param replacement if \code{pattern} is a character vector, then
#'   \code{replacement} must be character vector of equal length, for a 1:1
#'   match.  If \code{pattern} is a \link{dictionary}, then \code{replacement}
#'   should not be used.
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param verbose print status messages if \code{TRUE}
#' @export
#' @examples
#' mydfm <- dfm(data_corpus_irishbudget2010)
#'
#' # lemmatization
#' infle <- c("foci", "focus", "focused", "focuses", "focusing", "focussed", "focusses")
#' lemma <- rep("focus", length(infle))
#' mydfm2 <- dfm_replace(mydfm, infle, lemma)
#' featnames(dfm_select(mydfm2, infle))
#'
#' # stemming
#' feat <- featnames(mydfm)
#' stem <- char_wordstem(feat, "porter")
#' mydfm3 <- dfm_replace(mydfm, feat, stem, case_insensitive = FALSE)
#' identical(mydfm3, dfm_wordstem(mydfm, "porter"))
dfm_replace <- function(x, pattern, replacement = NULL, case_insensitive = TRUE, 
                           verbose = quanteda_options("verbose")) {
    UseMethod("dfm_replace")
}

#' @export
dfm_replace.default <- function(x, pattern, replacement = NULL, case_insensitive = TRUE, 
                                verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "dfm_replace"))
}
    
#' @export
dfm_replace.dfm <- function(x, pattern, replacement = NULL, case_insensitive = TRUE, 
                            verbose = quanteda_options("verbose")) {
    
    if (!length(pattern)) return(x)
    colnames(x) <- replace_type(featnames(x), pattern, replacement, case_insensitive)
    dfm_compress(x, 'features')
}
