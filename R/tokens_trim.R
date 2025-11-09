#' Trim tokens using frequency threshold-based feature selection
#'
#' @description Returns a tokens object reduced in size based on
#'   document and term frequency, usually in terms of a minimum frequency, but
#'   may also be in terms of maximum frequencies.  Setting a combination of
#'   minimum and maximum frequencies will select features based on a range.
#'
#' @inheritParams dfm_trim
#' @param padding if `TRUE`, leave an empty string where the removed tokens
#'   previously existed.
#' @return A [tokens] object with reduced size.
#' @export
#' @seealso [dfm_trim()]
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#'
#' # keep only words occurring >= 10 times and in >= 2 documents
#' tokens_trim(toks, min_termfreq = 10, min_docfreq = 2, padding = TRUE)
#'
#' # keep only words occurring >= 10 times and no more than 90% of the documents
#' tokens_trim(toks, min_termfreq = 10, max_docfreq = 0.9, docfreq_type = "prop",
#'             padding = TRUE)
#'
#' @export
tokens_trim <- function(x,
                     min_termfreq = NULL, max_termfreq = NULL, 
                     termfreq_type = c("count", "prop", "rank", "quantile"),
                     min_docfreq = NULL, max_docfreq = NULL, 
                     docfreq_type = c("count", "prop", "rank", "quantile"),
                     padding = FALSE,
                     verbose = quanteda_options("verbose")) {
    UseMethod("tokens_trim")
}

#' @export
tokens_trim.default <- function(x,
                             min_termfreq = NULL, max_termfreq = NULL,
                             termfreq_type = c("count", "prop", "rank", "quantile"),
                             min_docfreq = NULL, max_docfreq = NULL,
                             docfreq_type = c("count", "prop", "rank", "quantile"),
                             padding = FALSE,
                             verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_trim")
}

#' @export
tokens_trim.tokens_xptr <- function(x,
                                    min_termfreq = NULL, max_termfreq = NULL,
                                    termfreq_type = c("count", "prop", "rank", "quantile"),
                                    min_docfreq = NULL, max_docfreq = NULL,
                                    docfreq_type = c("count", "prop", "rank", "quantile"),
                                    padding = FALSE,
                                    verbose = quanteda_options("verbose")) {
    
    termfreq_type <- match.arg(termfreq_type)
    docfreq_type <- match.arg(docfreq_type)
    verbose <- check_logical(verbose)
    
    f <- trim_features(cpp_get_freq(x, no_padding = TRUE), 
                       cpp_get_freq(x, no_padding = TRUE, boolean = TRUE),
                       cpp_ndoc(x),
                       min_termfreq, max_termfreq, termfreq_type, 
                       min_docfreq, max_docfreq, docfreq_type)

    result <- tokens_select(x, f, valuetype = "fixed", case_insensitive = FALSE,
                            padding = padding, verbose = FALSE)
    
    # if (verbose) # TODO: print verbose message 
    #    print()
    
    return(result)
}

#' @export
tokens_trim.tokens <- function(x, ...) {
    as.tokens(tokens_trim(as.tokens_xptr(x), ...))
}
