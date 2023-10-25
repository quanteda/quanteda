#' Bootstrap a dfm
#'
#' Create an array of resampled dfms.
#' @param x a [dfm] object
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
#' dfmat <- dfm(tokens(txt))
#' bootstrap_dfm(dfmat, n = 3, verbose = TRUE)
#'
bootstrap_dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    UseMethod("bootstrap_dfm")
}

#' @export
bootstrap_dfm.default <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    check_class(class(x), "bootstrap_dfm")
}

#' @noRd
#' @export
#' @examples
#' # bootstrapping from a dfm
#' dfmat <- corpus_reshape(corpus(txt), to = "sentences") |>
#'     tokens() |>
#'     dfm()
#' bootstrap_dfm(dfmat, n = 3)
bootstrap_dfm.dfm <- function(x, n = 10, ..., verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    n <- check_integer(n, min = 0)
    verbose <- check_logical(verbose)

    if (verbose) {
        catm("Bootstrapping dfm to create multiple dfm objects...\n")
        catm("   ...resampling and forming dfms: 0", appendLF = FALSE)
    }
    result <- list()
    result[[1]] <- dfm_group(x, groups = docid(x), fill = TRUE)
    for (i in seq_len(n)) {
        if (verbose)
            catm(",", i, appendLF = FALSE)
        temp <- dfm_sample(x, size = NULL, replace = TRUE, by = docid(x))
        temp <- dfm_group(temp)
        result[[i + 1]] <- temp
    }
    catm("\n")
    names(result) <- paste0("dfm_", seq(0, n))
    if (verbose)
        catm("   ...complete.\n")
    class(result) <- "dfm_bootstrap"
    return(result)
}
