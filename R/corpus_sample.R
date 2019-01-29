#' Randomly sample documents from a corpus
#' 
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement.  Works just as \code{\link{sample}} works for the
#' documents and their associated document-level variables.
#' @param x a corpus object whose documents will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.  May not be applied when \code{by} is used.
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
corpus_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    UseMethod("corpus_sample")
}

#' @export
corpus_sample.default <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus_sample"))
}

#' @import data.table data.table
#' @export
corpus_sample.corpus <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    index <- docID <- temp <- NULL
    
    if (!is.null(by)) {
        if (!is.null(prob)) stop("prob not implemented with by")
        if (by == "document") by <- "_document"
        sample_index <- sample_bygroup(seq_len(ndoc(x)), group = docvars(x, by), replace = replace)
    } else {
        sample_index <- base::sample(ndoc(x), size, replace, prob) 
    }
        
    documents(x) <- documents(x)[sample_index, , drop = FALSE]
    if (is.corpuszip(x)) {
        texts(x) <- texts(x)[sample_index]
        x$docnames <- x$docnames[sample_index]
    }
    return(x)
}

# ---------- internal functions from older resample.R ---------

is.resampled <- function(x) { FALSE }

nresample <- function(x) { 0 }
