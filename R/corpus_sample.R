#' Randomly sample documents from a corpus
#'
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement, optionally by grouping variables or with probability
#' weights.
#' @param x a [corpus] object whose documents will be sampled
#' @param size a positive number, the number of documents to select; when used
#'   with `by`, the number to select from each group or a vector equal in
#'   length to the number of groups defining the samples to be chosen in each
#'   category of `by`.  By defining a size larger than the number of documents,
#'   it is possible to oversample when `replace = TRUE`.
#' @param replace if `TRUE`, sample  with replacement
#' @param prob a vector of probability weights for obtaining the elements of the
#'   vector being sampled.  May not be applied when `by` is used.
#' @param by optional grouping variable for sampling.  This will be evaluated in
#'   the docvars data.frame, so that docvars may be referred to by name without
#'   quoting.  This also changes previous behaviours for `by`.
#'   See `news(Version >= "2.9", package = "quanteda")` for details.
#' @return a [corpus] object (re)sampled on the documents, containing the document
#'   variables for the documents sampled.
#' @export
#' @keywords corpus
#' @examples
#' set.seed(123)
#' # sampling from a corpus
#' summary(corpus_sample(data_corpus_inaugural, size = 5))
#' summary(corpus_sample(data_corpus_inaugural, size = 10, replace = TRUE))
#'
#' # sampling with by
#' corp <- data_corpus_inaugural
#' corp$century <- paste(floor(corp$Year / 100) + 1)
#' corp$century <- paste0(corp$century, ifelse(corp$century < 21, "th", "st"))
#' corpus_sample(corp, size = 2, by = century) |>
#'     summary()
#' # needs drop = TRUE to avoid empty interactions
#' corpus_sample(corp, size = 1, by = interaction(Party, century, drop = TRUE), replace = TRUE) |>
#'     summary()
#'
#' # sampling sentences by document
#' corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
#'                  two = "First sentence, doc2.  Second sentence, doc2."),
#'                docvars = data.frame(var1 = c("a", "a"), var2 = c(1, 2)))
#' corpus_reshape(corp, to = "sentences") %>%
#'     corpus_sample(replace = TRUE, by = docid(.))
#'
#' # oversampling
#' corpus_sample(corp, size = 5, replace = TRUE)
corpus_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL) {
    UseMethod("corpus_sample")
}

#' @export
corpus_sample.default <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL) {
    check_class(class(x), "corpus_sample")
}

#' @export
corpus_sample.corpus <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    x <- as.corpus(x)

    if (!missing(by)) {
        by <- eval(substitute(by), get_docvars(x, user = TRUE, system = TRUE), parent.frame())
        if (is.factor(by)) by <- droplevels(by)
    }

    i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob, by = by)
    return(x[i])
}
