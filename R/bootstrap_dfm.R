#' bootstrap a dfm
#'
#' Create an array of resampled dfms.
#' @param x a character or \link{corpus} object
#' @param n number of resamples
#' @param ... additional arguments passed to \code{\link{dfm}}
#' @param verbose if \code{TRUE} print status messages
#' @details 
#' This code loops the creation of a dfm from a corpus, keeping a constant set of features based
#' on the original dfm.  Resampling of the corpus is done at the sentence level, within document.
#' @return 
#' A named list of \link{dfm} objects, where the first, \code{dfm_0}, is the dfm from the original
#' texts, and subsequent elements are the sentence-resampled dfms.
#' @author Kenneth Benoit
#' @export
#' @keywords dfm experimental bootstrap
#' @examples 
#' txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'          texttwo = "Premiere phrase.  Deuxieme phrase.")
#' bootstrap_dfm(txt, n = 3)         
bootstrap_dfm <- function(x, n = 10, ..., verbose = getOption("verbose")) {
    UseMethod("bootstrap_dfm")
}

#' @noRd
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ..., verbose = getOption("verbose")) {

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
        thisdfm <- dfm(corpus_sample(corp_sentences, replace = TRUE, by = "_document"), groups = "_document", ...)
        thisdfm <- dfm_select(thisdfm, result[[1]])
        result <- c(result, setNames(list(thisdfm), paste0("dfm_", i)))
    }
    if (verbose) message("") # adds LF
    
    class(result) <- c("dfm_bootstrap", class(result))
    result
}

#' @noRd
#' @export
bootstrap_dfm.character <- function(x, n = 10, ..., verbose = TRUE) {
    bootstrap_dfm(corpus(x), ..., verbose = verbose)
}



