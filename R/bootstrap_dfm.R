#' Bootstrap a dfm
#'
#' Create an array of resampled dfms.
#' @param x a character or [corpus] object
#' @param n number of resamples
#' @param ... additional arguments passed to [dfm()]
#' @param verbose if `TRUE` print status messages
#' @details Function produces multiple, resampled [dfm] objects, based on
#'   resampling sentences (with replacement) from each document, recombining
#'   these into new "documents" and computing a dfm for each. Resampling of
#'   sentences is done strictly within document, so that every resampled
#'   document will contain at least some of its original tokens.
#' @return A named list of [dfm] objects, where the first, `dfm_0`, is
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
#' @importFrom stringi stri_replace_all_fixed
#' @export
bootstrap_dfm.corpus <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    if (verbose)
        message("Segmenting the ",
                stri_replace_all_fixed(as.character(sys.calls()[2][[1]])[1],
                                       "bootstrap_dfm.", ""),
                " into sentences...", appendLF = FALSE)
    x <- as.corpus(x)
    x <- corpus_reshape(x, to = "sentences")
    if (verbose) message("done.")
    bootstrap_dfm(dfm(x, ...),  n = n, ..., verbose = verbose)
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
    x <- as.dfm(x)
    group <- get_docvars(x, "docid_", system = TRUE, drop = TRUE)
    if (length(levels(group)) == ndoc(x))
        stop("x must contain more than one row per document")

    if (verbose) {
        message("Bootstrapping the sentences to create multiple dfm objects...")
        message("   ...resampling and forming dfms: 0", appendLF = FALSE)
    }
    result <- list()
    result[[1]] <- dfm_group(x, groups = "docid_", fill = TRUE)
    for (i in seq_len(n)) {
        if (verbose)
            message(", ", i, appendLF = FALSE)
        temp <- x[sample_bygroup(seq_len(ndoc(x)), get_docvars(x, "docid_", system = TRUE, drop = TRUE),
                                 replace = TRUE), ]
        temp <- dfm_group(temp)
        result[[i + 1]] <- temp
    }
    names(result) <- paste0("dfm_", seq(0, n))
    if (verbose)
        message("\n   ...complete.\n")
    class(result) <- c("dfm_bootstrap")
    return(result)
}
