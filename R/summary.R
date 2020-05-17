#' Summarize a corpus
#'
#' Displays information about a corpus, including attributes and metadata such
#' as date of number of texts, creation and source.
#'
#' @param object corpus to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param showmeta set to `TRUE` to include document-level
#'   meta-data
#' @param tolower convert texts to lower case before counting types
#' @param ... additional arguments passed through to [tokens()]
#' @rdname quanteda-summary
#' @export
#' @method summary corpus
#' @keywords internal corpus
#' @examples
#' corp <- data_corpus_inaugural
#' summary(corp, cache = TRUE)
#' toks <- tokens(corp)
#' summary(toks, cache = TRUE)
#' dfmat <- dfm(toks)
#' summary(dfmat, cache = TRUE)
summary.corpus <- function(object, cache = TRUE, ...) {
    summarize(as.corpus(object), cache, ...)
}

#' @method summary tokens
#' @export
summary.tokens <- function(object, cache = TRUE, ...) {
    summarize(as.tokens(object), cache, ...)
}

#' @method summary dfm
#' @export
summary.dfm <- function(object, cache = TRUE, ...) {
    summarize(as.dfm(object), cache, ...)
}

summarize <- function(x, cache = TRUE, ...) {
    if (cache) {
        result <- get_cache(x, "summary", ...)
        if (!is.null(result))
            return(result)
    } else {
        clear_cache(x, "summary")
    }
    
    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))
    temp <- dfm(x, ...)
    result <- convert(
        dfm_lookup(temp, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_token <- ntoken(temp)
    result$n_type <- nfeat(temp) 
    result$noise <- result$any / result$n_token
    result$n_sent <- NA
    result$is_dup <- NA
    
    if (is.corpus(x))
        result$n_sent <- ntoken(tokens(x, what = "sentence"))
    if (is.corpus(x) || is.tokens(x))
        result$is_dup <- duplicated(x)
    
    if (cache)
        set_cache(x, "summary", result, ...)
    return(result)
}
