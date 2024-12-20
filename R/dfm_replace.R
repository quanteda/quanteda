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
#' @inheritParams messages
#' @export
#' @examples
#' dfmat1 <- dfm(tokens(data_corpus_inaugural))
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
    check_class(class(x), "dfm_replace")
}
    
#' @export
dfm_replace.dfm <- function(x, pattern, replacement, case_insensitive = TRUE,
                            verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    pattern <- check_character(pattern, min_len = 0, max_len = Inf, strict = TRUE)
    replacement <- check_character(replacement, min_len = 0, max_len = Inf, strict = TRUE)
    case_insensitive <- check_logical(case_insensitive)
    
    if (length(pattern) != length(replacement))
        stop("The length of pattern and replacement must be the same", call. = FALSE)
    if (!length(pattern)) return(x)

    if (!length(pattern)) return(x)
    
    if (verbose)
        before <- stats_dfm(x)
    set_dfm_featnames(x) <- replace_type(featnames(x), pattern, replacement, case_insensitive)
    x <- dfm_compress(x, "features")
    if (verbose)
        message_dfm("dfm_replace()", before, stats_dfm(x))
    return(x)
}
