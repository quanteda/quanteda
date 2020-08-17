#' Summarize documents
#'
#' Count the total number of number tokens and sentences.
#'
#' Count the total number of characters, tokens and sentences as well as special
#' tokens such as numbers, punctuation marks, symbols, tags and emojis.
#' \itemize{ 
#' \item chars = number of characters; equal to [nchar()] 
#' \item sents
#' = number of sentences; equal `ntoken(tokens(x), what = "sentence")` 
#' \item
#' tokens = number of tokens; equal to [ntoken()] 
#' \item types = number of unique tokens; equal to [ntype()] 
#' \item puncts = number of punctuation marks (`^\p{P}+$`) 
#' \item numbers = number of numeric tokens
#' (`^\p{Sc}{0,1}\p{N}+([.,]*\p{N})*\p{Sc}{0,1}$`) 
#' \item symbols = number of symbols (`^\p{S}$`) 
#' \item tags = number of tags; sum of `pattern_username` and `pattern_hashtag` 
#' in [quanteda_options()] 
#' \item emojis = number of emojis (`^\p{Emoji_Presentation}+$`) 
#' }
#' @param x corpus to be summarized
#' @param cache if `TRUE`, use internal cache from the second time.
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
    patterns[["emoji"]] <- "^\\p{Emoji_Presentation}+$"
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
