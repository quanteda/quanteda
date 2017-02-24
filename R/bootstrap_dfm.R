#' bootstrap a dfm
#'
#' Create an array of resampled dfms.
#' @param x the dfm
#' @param n number of resamples
#' @param ... additional arguments passed to \link{\code{dfm}}
#' @param verbose if \code{TRUE} print status messages
#' @export
bootstrap_dfm <- function(x, n = 10, ..., verbose = TRUE) {
    UseMethod("dfm_bootstrap")
}

#' @noRd
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ..., verbose = TRUE) {

    if (verbose) 
        message("Bootstrapping the corpus to create multiple dfm objects...")
    
    if (verbose) 
        message("   ...segmenting the corpus into sentences")
    corp_sentences <- corpus_reshape(x, to = "sentences")
    
    if (verbose)
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    result <- list(dfm_0 = dfm(x, ...))
    
    # could be parallelized
    for (i in 1:n) {
        if (verbose) message(", ", i, appendLF = FALSE)
        result <- c(result, 
                    as.list(setNames(dfm(corpus_resample(corp_sentences, replace = TRUE), groups = "_document", ...)), 
                            paste0("dfm_", i)))
    }
    if (verbose) message("") # adds LF
    
    result
}

#' @noRd
#' @export
bootstrap_dfm.character <- function(x, n = 10, ..., verbose = TRUE) {
    dfm_bootstrap(corpus(x), ..., verbose = verbose)
}



