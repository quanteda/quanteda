#' Summarize a corpus
#'
#' Displays information about a corpus, including attributes and metadata such
#' as date of number of texts, creation and source.
#'
#' @param object an object to be summarized.
#' @param ... not used.
#' @return a invisible list of summary statistics.
#' @examples
#' corp <- data_corpus_inaugural
#' summary(corp)
#' toks <- tokens(data_corpus_inaugural)
#' summary(toks)
#' dfmt <- dfm(toks)
#' summary(dfmt)
#' fcmt <- fcm(dfmt)
#' summary(fcmt)
#' @export
#' @method summary corpus
summary.corpus <- function(object, ...) {
    invisible(summary_corpus(as.corpus(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary tokens
summary.tokens <- function(object, ...) {
    invisible(summary_tokens(as.tokens(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary dfm
summary.dfm <- function(object, ...) {
    invisible(summary_dfm(as.dfm(object)))
}

#' @rdname summary.corpus
#' @export
#' @method summary fcm
summary.fcm <- function(object, ...) {
    invisible(summary_fcm(as.fcm(object)))
}


summarize <- function(x, tolower = FALSE, ...) {
    patterns <- removals_regex(punct = TRUE, symbols = TRUE,
                               numbers = TRUE, url = TRUE)
    patterns[["tag"]] <-
        list("username" = paste0("^", quanteda_options("pattern_username"), "$"),
             "hashtag" = paste0("^", quanteda_options("pattern_hashtag"), "$"))
    patterns[["emoji"]] <- "^\\p{Emoji_Presentation}+$"
    dict <- dictionary(patterns)

    y <- dfm(tokens(x, ...), tolower = tolower)
    temp <- convert(
        quanteda::dfm_lookup(y, dictionary = dict, valuetype = "regex", levels = 1),
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
        result$chars <- stringi::stri_length(x)
        result$sents <- lengths(tokenize_sentence(x))
    }

    return(result)
}
