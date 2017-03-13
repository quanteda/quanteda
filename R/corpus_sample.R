#' randomly sample documents from a corpus
#' 
#' Takes a random sample or documents or features of the specified size from a 
#' corpus or document-feature matrix, with or without replacement.  Works just 
#' as \code{\link{sample}} works for the documents and their associated 
#' document-level variables.
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
#' summary(corpus_sample(data_corpus_inaugural, 10, replace=TRUE))
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


#' @rdname corpus_sample
#' @noRd
#' @import data.table data.table
#' @export
corpus_sample.corpus <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL, ...) {
    index <- docID <- temp <- NULL
    
    if (!is.null(by)) {
        if (by == "document") by <- "_document"
        dt <- data.table(index = 1:ndoc(x), docID = docvars(x, by))
        dt[, temp := sample(1:.N, replace = TRUE), by = docID]
        dt[, sample_index := index[temp], by = docID]
        sample_index <- dt[, sample_index]
    } else {
        sample_index <- base::sample(ndoc(x), size, replace, prob) 
    }
        
    documents(x) <- documents(x)[sample_index, , drop = FALSE]
    if (is.corpuszip(x)) {
        texts(x) <- texts(x)[sample_index]
        x$docnames <- x$docnames[sample_index]
    }
    x
}

