#' Randomly sample documents from a corpus
#' 
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement.  Works just as [sample()] works for the
#' documents and their associated document-level variables.
#' @param x a corpus object whose documents will be sampled
#' @param size a positive number, the number of documents to select; when used
#'   with groups, the number to select from each group or a vector equal in
#'   length to the number of groups defining the samples to be chosen in each
#'   group category.  By defining a size larger than the number of documents, it
#'   is possible to *over*sample groups.
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.  May not be applied when `by` is used.
#' @param by a grouping variable for sampling.  Useful for resampling
#'   sub-document units such as sentences, for instance by specifying `by =
#'   "document"`
#' @return A corpus object with number of documents equal to `size`, drawn 
#'   from the corpus `x`.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @export
#' @keywords corpus
#' @examples
#' set.seed(2000)
#' # sampling from a corpus
#' summary(corpus_sample(data_corpus_inaugural, 5))
#' summary(corpus_sample(data_corpus_inaugural, 10, replace = TRUE))
#' 
#' # sampling sentences within document
#' corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
#'                       two = "First sentence, doc2.  Second sentence, doc2."))
#' corpsent <- corpus_reshape(corp, to = "sentences")
#' texts(corpsent)
#' texts(corpus_sample(corpsent, replace = TRUE, by = "document"))
corpus_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    UseMethod("corpus_sample")
}

#' @export
corpus_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    stop(friendly_class_undefined_message(class(x), "corpus_sample"))
}

#' @export
corpus_sample.corpus <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    x <- as.corpus(x)
    if (!is.null(by)) {
        if (!is.null(prob)) stop("prob not implemented with by")
        if (by == "document") by <- "docid_"
        i <- sample_bygroup(seq_len(ndoc(x)), 
                            group = get_docvars(x, by, system = TRUE, drop = TRUE), 
                            size = size, replace = replace)
    } else {
        if (is.null(size)) size <- ndoc(x)
        i <- base::sample(ndoc(x), size, replace, prob) 
    }
    
    return(x[i])
}
