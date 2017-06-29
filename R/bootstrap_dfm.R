#' bootstrap a dfm
#' 
#' Create an array of resampled dfms.
#' @param x a character or \link{corpus} object
#' @param n number of resamples
#' @param ... additional arguments passed to \code{\link{dfm}}
#' @param verbose if \code{TRUE} print status messages
#' @details Function produces multiple, resampled \link{dfm} objects, from a
#'   common source,  keeping a constant set of features based on the original
#'   dfm.  From a corpus or character source, resampling is done at the sentence
#'   level, within document.  From a dfm source, resampling is done at the
#'   feature level using a multinominal sampling strategy, keeping document
#'   marginals constant.
#' @return A named list of \link{dfm} objects, where the first, \code{dfm_0}, is
#'   the dfm from the original texts, and subsequent elements are the 
#'   sentence-resampled dfms.
#' @author Kenneth Benoit
#' @export
#' @keywords dfm experimental bootstrap
#' @examples 
#' # bootstrapping from the original text
#' txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'          texttwo = "Premiere phrase.  Deuxieme phrase.")
#' bootstrap_dfm(txt, n = 3)         
bootstrap_dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    UseMethod("bootstrap_dfm")
}

#' @noRd
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {

    if (verbose) 
        message("Bootstrapping the corpus to create multiple dfm objects...")
    
    if (verbose) 
        message("   ...segmenting the corpus into sentences")
    corp_sentences <- corpus_reshape(x, to = "sentences")
    
    result <- list()
    if (verbose)
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    
    result[['dfm_0']] <- dfm(x, ...)
    # could be parallelized
    for (i in seq_len(n)) {
        if (verbose) message(", ", i, appendLF = FALSE)
        temp <- dfm(corpus_sample(corp_sentences, replace = TRUE, by = "_document"), 
                    groups = "_document", ...)
        result[[paste0("dfm_", i)]] <- dfm_select(temp, result[[1]])
    }
    if (verbose) 
        message("\n   ... complete.")

    class(result) <- c("dfm_bootstrap")
    return(result)
}

#' @noRd
#' @export
bootstrap_dfm.character <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    bootstrap_dfm(corpus(x), n = n, ..., verbose = verbose)
}


#' @noRd
#' @export
#' @examples 
#' # bootstrapping from a dfm
#' mydfm <- dfm(corpus_reshape(corpus(txt), to = "sentences"))
#' bootstrap_dfm(mydfm, n = 3)
bootstrap_dfm.dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    
    if (verbose) 
        message("Bootstrapping the dfm to create multiple dfm objects...")
    if (verbose)
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    
    result <- list()
    
    # construct the original dfm
    result[['dfm_0']] <- dfm_group(x, groups = docvars(x, '_document'))
    
    # random sample dfm
    for (i in seq_len(n)) {
        if (verbose) message(", ", i, appendLF = FALSE)
        temp <- x[sample(seq_len(ndoc(x)), ndoc(x), replace = TRUE),]
        temp <- dfm_group(temp, groups = docvars(temp, '_document'))
        result[[paste0("dfm_", i)]] <- dfm_select(temp, result[[1]])
    }
    if (verbose) 
        message("\n   ... complete.")
    
    class(result) <- c("dfm_bootstrap")
    return(result)
}

# microbenchmark(
#     bootstrap_dfm(txt, n = 3),
#     bootstrap_dfm( dfm(corpus_reshape(corpus(txt), to = "sentences")), n = 3 ),
#     times = 50
# )
