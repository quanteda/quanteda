#' Randomly sample documents from a corpus
#' 
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement.  Works just as \code{\link{sample}} works for the
#' documents and their associated document-level variables.
#' @param x a corpus object whose documents will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param by a grouping variable for sampling.  Useful for resampling
#'   sub-document units such as sentences, for instance by specifying \code{by =
#'   "document"}
#' @param ... unused
#' @return A corpus object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @export
#' @keywords corpus
#' @examples
#' # sampling from a corpus
#' summary(corpus_sample(data_corpus_inaugural, 5))
#' summary(corpus_sample(data_corpus_inaugural, 10, replace = TRUE))
#' 
#' # sampling sentences within document
#' doccorpus <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
#'                       two = "First sentence, doc2.  Second sentence, doc2."))
#' sentcorpus <- corpus_reshape(doccorpus, to = "sentences")
#' texts(sentcorpus)
#' texts(corpus_sample(sentcorpus, replace = TRUE, by = "document"))
corpus_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    UseMethod("corpus_sample")
}

#' @export
corpus_sample.default <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus_sample"))
}

#' @export
corpus_sample.corpus <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    x <- as.corpus(x)
    if (!is.null(by)) {
        if (by == "document") by <- "_docnum"
        docvar <- attr(x, "docvars")
        index <- unlist(lapply(split(seq_len(ndoc(x)), docvar[[by]]), base::sample), 
                        use.names = FALSE)
    } else {
        index <- base::sample(ndoc(x), size, replace, prob) 
    }
    x[index]
}
