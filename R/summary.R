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
    object <- as.corpus(object)
    if (cache) {
        result <- get_cache(object, "summary", ...)
        if (!is.null(result))
            return(result)
    } else {
        clear_cache(object, "summary")
    }
    
    result <- summary.tokens(tokens(object), ...)
    result$n_sent <- ntoken(tokens(object, what = "sentence"))
    result$is_dup <- duplicated(object)
    
    if (cache)
        set_cache(object, "summary", result, ...)
    return(result)
}

#' @method summary tokens
#' @export
summary.tokens <- function(object, cache = TRUE, ...) {
    object <- as.tokens(object)
    if (cache) {
        result <- get_cache(object, "summary", ...)
        if (!is.null(result))
            return(result)
    } else {
        clear_cache(object, "summary")
    }
    
    result <- summary.dfm(dfm(object), ...)
    result$n_sent <- NA
    result$is_dup <- duplicated(object)
    
    if (cache)
        set_cache(object, "summary", result, ...)
    return(result)
}

#' @method summary dfm
#' @export
summary.dfm <- function(object, cache = TRUE, ...) {
    object <- as.dfm(object)
    if (cache) {
        result <- get_cache(object, "summary", ...)
        if (!is.null(result))
            return(result)
    } else {
        clear_cache(object, "summary")
    }
    
    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))
    result <- convert(
        dfm_lookup(object, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_token <- ntoken(object)
    result$n_type <- nfeat(object) 
    result$noise <- result$any / result$n_token
    result$n_sent <- NA
    result$is_dup <- NA
    
    if (cache)
        set_cache(object, "summary", result, ...)
    return(result)
}
