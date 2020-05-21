#' Summarize documents
#'
#' Count the total number of number tokens and sentences.
#'
#' Count the total number tokens and sentences as well as
#' tokens with numbers, punctuation marks or symbols.
#' \itemize{
#'   \item n_token = number of tokens; equal to [ntoken()]
#'   \item n_type = number of unique tokens; equal to [ntype()]
#'   \item n_sent = number of sentences; equal `ntoken(tokens(x), what = "sentence")`
#'   \item number = number of tokens with numbers (`\p{N}`)
#'   \item punct = number of tokens with numbers (`\p{P}`)
#'   \item symbol = number of tokens with numbers (`\p{S}`)
#'   \item any = number of tokens with numbers (`[\p{N}\p{P}\p{S}]`)
#'   \item noise = \eqn{any / n_token}
#'   \item is_dup = existence of identical documents
#' }
#' @param x corpus to be summarized
#' @param cache if `TRUE`, use internal cache from the second time
#' @param ... additional arguments passed through to [dfm()]
#' @export
#' @keywords textstat
#' @examples
#' corp <- data_corpus_inaugural
#' textstat_summary(corp, cache = TRUE)
#' toks <- tokens(corp)
#' textstat_summary(toks, cache = TRUE)
#' dfmat <- dfm(toks)
#' textstat_summary(dfmat, cache = TRUE)
textstat_summary <- function(x, cache = TRUE, ...) {
    UseMethod("textstat_summary")
}

#' @export
textstat_summary.default <- function(x, cache = TRUE, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_summary"))
}

#' @method textstat_summary corpus
#' @export
textstat_summary.corpus <- function(x, cache = TRUE, ...) {
    summarize(as.corpus(x), cache, ...)
}

#' @method textstat_summary tokens
#' @export
textstat_summary.tokens <- function(x, cache = TRUE, ...) {
    summarize(as.tokens(x), cache, ...)
}

#' @method textstat_summary dfm
#' @export
textstat_summary.dfm <- function(x, cache = TRUE, ...) {
    summarize(as.dfm(x), cache, ...)
}

summarize <- function(x, cache = TRUE, ...) {
    if (cache) {
        result <- get_cache(x, "summary", ...)
        if (!is.null(result))
            return(result)
    } else {
        clear_cache(x, "summary")
    }
    
    patterns <- removals_regex(punct = TRUE, symbols = TRUE, 
                               numbers = TRUE, url = TRUE)
    patterns[["tag"]] <- list("username" = paste0("^", quanteda_options("pattern_username"), "$"),
                              "hashtag" = paste0("^", quanteda_options("pattern_hashtag"), "$"))
    patterns[["emoji"]] <- "^[\\p{Emoji_Presentation}]+$"
    dict <- dictionary(patterns)
    
    y <- dfm(x, ...)
    temp <- convert(
        dfm_lookup(y, dictionary = dict, valuetype = "regex", levels = 1),
        "data.frame",
        docid_field = "document"
    )
    result <- data.frame(
        "document" = docnames(y),
        "chars" = NA,
        "sents" = NA,
        "tokens" = ntoken(y),
        "types" = ntype(y),
        "puncts" = as.integer(temp$punct),
        "numbers" = as.integer(temp$numbers),
        "symbols" = as.integer(temp$symbols),
        "urls" = as.integer(temp$url),
        "tags" = as.integer(temp$tag),
        "emojis" = as.integer(temp$emoji),
        row.names = seq_len(ndoc(y)),
        stringsAsFactors = FALSE
    )
    
    if (is.corpus(x)) {
        result$chars <- nchar(x)
        result$sents <- ntoken(tokens(x, what = "sentence"))
    }

    if (cache)
        set_cache(x, "summary", result, ...)
    return(result)
}
