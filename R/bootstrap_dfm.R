#' Bootstrap a dfm
#' 
#' Create an array of resampled dfms.
#' @param x a character or \link{corpus} object
#' @param n number of resamples
#' @param ... additional arguments passed to \code{\link{dfm}}
#' @param verbose if \code{TRUE} print status messages
#' @details Function produces multiple, resampled \link{dfm} objects, based on 
#'   resampling sentences (with replacement) from each document, recombining
#'   these into new "documents" and computing a dfm for each. Resampling of
#'   sentences is done strictly within document, so that every resampled
#'   document will contain at least some of its original tokens.
#' @return A named list of \link{dfm} objects, where the first, \code{dfm_0}, is
#'   the dfm from the original texts, and subsequent elements are the 
#'   sentence-resampled dfms.
#' @author Kenneth Benoit
#' @export
#' @keywords dfm experimental bootstrap
#' @examples 
#' # bootstrapping from the original text
#' set.seed(10)
#' txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'          texttwo = "Premiere phrase.  Deuxieme phrase.")
#' bootstrap_dfm(txt, n = 3, verbose = TRUE)
#'          
bootstrap_dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    UseMethod("bootstrap_dfm")
}

#' @export
bootstrap_dfm.default <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "bootstrap_dfm"))
}

#' @noRd
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    if (verbose)
        message("Segmenting the ",
                stri_replace_all_fixed(as.character(sys.calls()[2][[1]])[1],
                                       "bootstrap_dfm.", ""),
                " into sentences...", appendLF = FALSE)
    corp_sentences <- corpus_reshape(x, to = "sentences")
    if (verbose) message("done.")
    result <- bootstrap_dfm(dfm(corp_sentences, ...),  n = n, ..., verbose = verbose)
    # replace original dfm with the one created pre-splitting
    result[["dfm_0"]] <- dfm(x, ...)
    result
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
#' dfmat <- dfm(corpus_reshape(corpus(txt), to = "sentences"))
#' bootstrap_dfm(dfmat, n = 3)
bootstrap_dfm.dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {

    if (! "_document" %in% names(x@docvars))
        stop("x must be a dfm with a _document field")
    if (length(unique(docvars(x, "_document"))) == ndoc(x))
        stop("x must contain more than one row per document")

    if (verbose) {
        message("Bootstrapping the sentences to create multiple dfm objects...")
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    }

    x <- as.dfm(x)
    result <- list()
    # construct the original dfm
    result[["dfm_0"]] <- dfm_group(x, groups = docvars(x, "_document"))

    # randomly resample dfm indexes within document and recompile sampled sentences
    id <- index <- NULL
    for (i in seq_len(n)) {
        if (verbose) message(", ", i, appendLF = FALSE)
        temp <- x[sample_bygroup(seq_len(ndoc(x)), group = docvars(x, "_document"), replace = TRUE), ]
        temp <- dfm_group(temp, groups = docvars(temp, "_document"))
        result[[paste0("dfm_", i)]] <- temp
    }
    if (verbose) message("\n   ...complete.\n")

    class(result) <- c("dfm_bootstrap")
    return(result)
}
