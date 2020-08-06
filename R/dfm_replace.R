#' Replace features in dfm
#'
#' Substitute features based on vectorized one-to-one matching for lemmatization
#' or user-defined stemming.
#' @param x [dfm] whose features will be replaced
#' @param pattern a character vector.  See [pattern]
#'   for more details.
#' @param replacement if `pattern` is a character vector, then
#'   `replacement` must be character vector of equal length, for a 1:1
#'   match.
#' @inheritParams valuetype
#' @param verbose print status messages if `TRUE`
#' @export
#' @examples
#' dfmat1 <- dfm(data_corpus_inaugural)
#'
#' # lemmatization
#' taxwords <- c("tax", "taxing", "taxed", "taxed", "taxation")
#' lemma <- rep("TAX", length(taxwords))
#' featnames(dfm_select(dfmat1, pattern = taxwords))
#' dfmat2 <- dfm_replace(dfmat1, pattern = taxwords, replacement = lemma)
#' featnames(dfm_select(dfmat2, pattern = taxwords))
#'
#' # stemming
#' feat <- featnames(dfmat1)
#' featstem <- char_wordstem(feat, "porter")
#' dfmat3 <- dfm_replace(dfmat1, pattern = feat, replacement = featstem, case_insensitive = FALSE)
#' identical(dfmat3, dfm_wordstem(dfmat1, "porter"))
dfm_replace <- function(x, pattern, replacement, case_insensitive = TRUE,
                           verbose = quanteda_options("verbose")) {
    UseMethod("dfm_replace")
}

#' @export
dfm_replace.default <- function(x, pattern, replacement, case_insensitive = TRUE,
                                verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "dfm_replace"))
}
    
#' @export
dfm_replace.dfm <- function(x, pattern, replacement, case_insensitive = TRUE,
                            verbose = quanteda_options("verbose")) {

    if (length(pattern) != length(replacement))
        stop("Lengths of 'pattern' and 'replacement' must be the same")

    if (!length(pattern)) return(x)
    set_dfm_featnames(x) <- replace_type(featnames(x), pattern, replacement, case_insensitive)
    dfm_compress(x, "features")
}
