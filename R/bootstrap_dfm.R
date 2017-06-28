#' bootstrap a dfm
#' 
#' Create an array of resampled dfms.
#' @param x a character or \link{corpus} object
#' @param n number of resamples
#' @param ... additional arguments passed to \code{\link{dfm}}
#' @param verbose if \code{TRUE} print status messages
#' @details This code loops the creation of a dfm from a corpus, keeping a
#' constant set of features based on the original dfm.  Resampling of the corpus
#' is done at the sentence level, within document.
#' @return A named list of \link{dfm} objects, where the first, \code{dfm_0}, is
#' the dfm from the original texts, and subsequent elements are the
#' sentence-resampled dfms.
#' @author Kenneth Benoit
#' @export
#' @keywords dfm experimental bootstrap
#' @examples 
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

    result <- list()
    if (verbose)
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    
    result[['dfm_0']] <- dfm(x, ...)
    # could be parallelized
    corp_sentences <- corpus_reshape(x, to = "sentences")
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
#' txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'          texttwo = "Premiere phrase.  Deuxieme phrase.")
#' corp <- corpus_reshape(corpus(txt), to = "sentences")
#' mx <- dfm(corp)
#' 
bootstrap_dfm.dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    
    if (verbose) 
        message("Bootstrapping the dfm to create multiple dfm objects...")
    if (verbose)
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    
    result <- list()
    
    # extract metadata
    is_first <- docvars(x, '_segid') == 1
    vars <- docvars(x)[is_first,]
    names <- docvars(x, '_document')[is_first]
    
    # reconstruct the original dfm
    temp <- dfm_group(x, groups = docvars(x, '_docid'))
    rownames(temp) <- rownames(vars) <- names
    docvars(temp) <- vars
    result[['dfm_0']] <- temp
    
    # random sample dfm
    for (i in seq_len(n)) {
        if (verbose) message(", ", i, appendLF = FALSE)
        temp <- x[sample(seq_len(ndoc(x)), ndoc(x), replace = TRUE),]
        temp <- dfm_group(temp, groups = docvars(temp, '_docid'))
        rownames(temp) <- names
        result[[paste0("dfm_", i)]] <- dfm_select(temp, result[[1]])
    }
    if (verbose) 
        message("\n   ... complete.")
    
    class(result) <- c("dfm_bootstrap")
    return(result)
}



