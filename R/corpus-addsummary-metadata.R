#' Functions to add or retrieve corpus summary metadata
#'
#' @name summary_metadata
#' @aliases add_summary_metadata
#' @param x [corpus] object
#' @param ... additional arguments passed to [tokens()] when computing the
#'   summary information
#' @return `add_summary_metadata()` returns a corpus with summary metadata added
#'   as a data.frame, with the top-level list element names `summary`.
#' @details This is provided so that a [corpus] object can be stored with
#'   summary information to avoid having to compute this every time
#'   `[summary.corpus()]` is called.
#'   
#'   So in future calls, if `!is.null(meta(x, "summary", type = "system") &&
#'   !length(list(...))`, then `summary.corpus()` will simply return
#'   `get_system_meta()` rather than compute the summary statistics on the fly,
#'   which requires tokenizing the text.
#' @keywords corpus internal
#' @examples
#' corp <- corpus(data_char_ukimmig2010)
#' corp <- quanteda:::add_summary_metadata(corp)
#' quanteda:::get_summary_metadata(corp)
add_summary_metadata <- function(x, extended = FALSE, ...) {
    meta_system(x, "summary") <- summary(x, n = ndoc(x), showmeta = FALSE, ...)
    if (extended) {
        meta_system(x, "summary_extended") <- summarize_texts_extended(x)
    }
    return(x)
}

#' @rdname summary_metadata
#' @return `get_summary_metadata()` returns the summary metadata as a data.frame.
get_summary_metadata <- function(x, ...) {
    result <- meta(x, "summary", type = "system")
    if (!identical(docnames(x), result$Text)) {
        # warning("documents have changed; computing summary")
        meta_system(x, "summary") <- NULL
        result <- summary(x, n = ndoc(x), showmeta = FALSE, ...)
    }
    result
}

#' @rdname summary_metadata
#' @return `summarize_texts_extended()` returns extended summary information.
#' @examples 
#'
#' \dontrun{
#' # using extended summary
#' 
#' extended_data <- quanteda:::summarize_texts_extended(data_corpus_inaugural)
#' 
#' textplot_wordcloud(extended_data$top_dfm, max_words = 100)
#' 
#' library("ggplot2")
#' ggplot(data.frame(all_tokens = extended_data$all_tokens), aes(x = all_tokens)) +
#'    geom_histogram(color = "darkblue", fill = "lightblue") +
#'    xlab("Total length in tokens")
#' }
summarize_texts_extended <- function(x, stop_words = stopwords("en"), n = 100) {
    toks <- tokens(x) %>%
        tokens_tolower()
    
    # total tokens
    ndocs <- ndoc(x)
    ntoksall <- ntoken(toks)
    ntoks <- sum(ntoksall)

    # punctuation
    toks <- tokens(toks, remove_punct = TRUE, remove_symbols = FALSE)
    npunct <- ntoks - sum(ntoken(toks))
    
    # symbols and emoji
    toks <- tokens(toks, remove_symbols = TRUE)
    nsym <- ntoks - npunct - sum(ntoken(toks))
    
    # numbers
    toks <- tokens(toks, remove_numbers = TRUE)
    nnumbers <- ntoks - npunct - nsym - sum(ntoken(toks))
    
    # words
    nwords <- ntoks - npunct - nsym - nnumbers
    
    # stopwords
    dfmat <- dfm(toks)
    nfeats <- nfeat(dfmat)
    dfmat <- dfm_remove(dfmat, stop_words)
    nstopwords <- nfeats - nfeat(dfmat)
    
    # top n features
    top_words <- topfeatures(dfmat, n)
    
    # top n features as a dfm
    top_dfm <- dfm_select(dfmat, names(top_words)) %>%
        dfm_group(groups = rep(1, ndoc(dfmat)))
    
    list(total_tokens = ntoks,
         all_tokens = ntoksall,
         total_punctuation = npunct,
         total_symbols = nsym,
         total_numbers = nnumbers,
         total_words = nwords,
         total_stopwords = nstopwords,
         top_dfm = top_dfm)
}
