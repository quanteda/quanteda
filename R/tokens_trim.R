#' Trim a tokens using frequency threshold-based feature selection
#'
#' @description Returns a tokens object reduced in size based on
#'   document and term frequency, usually in terms of a minimum frequency, but
#'   may also be in terms of maximum frequencies.  Setting a combination of
#'   minimum and maximum frequencies will select features based on a range.
#'
#' @inheritParams dfm_trim
#' @return A [tokens] object with reduced size.
#' @export
#' @seealso [dfm_trim()]
#' @examples
#' dfmat <- dfm(tokens(data_corpus_inaugural))
#'
#' # keep only words occurring >= 10 times and in >= 2 documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 2)
#'
#' # keep only words occurring >= 10 times and in at least 0.4 of the documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 0.4)
#'
#' # keep only words occurring <= 10 times and in <=2 documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 2)
#'
#' # keep only words occurring <= 10 times and in at most 3/4 of the documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 0.75)
#'
#' # keep only words occurring 5 times in 1000, and in 2 of 5 of documents
#' dfm_trim(dfmat, min_docfreq = 0.4, min_termfreq = 0.005, termfreq_type = "prop")
#'
#' ## quantiles
#' toks <- as.tokens(list(unlist(mapply(rep, letters[1:10], 10:1), use.names = FALSE)))
#' dfmat <- dfm(toks)
#' dfmat
#' 
#' # keep only the top 20th percentile or higher features
# dfm_trim(dfmat, min_termfreq = 0.2, termfreq_type = "quantile", verbose = TRUE)
#'
#' # keep only words above the 80th percentile
#' dfm_trim(dfmat, min_termfreq = 0.800001, termfreq_type = "quantile", verbose = TRUE)
#' 
#' # keep only words occurring frequently (top 20%) and in <=2 documents
#' dfm_trim(dfmat, min_termfreq = 0.2, max_docfreq = 2, termfreq_type = "quantile")
#'
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
