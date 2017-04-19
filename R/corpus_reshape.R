#' recast the document units of a corpus
#' 
#' For a corpus, reshape (or recast) the documents to a different level of aggregation.  
#' Units of aggregation can be defined as documents, paragraphs, or sentences.
#' Because the corpus object records its current "units" status, it is possible
#' to move from recast units back to original units, for example from documents,
#' to sentences, and then back to documents (possibly after modifying the sentences).
#' @param x corpus whose document units will be reshaped
#' @param to new document units in which the corpus will be recast
#' @param ... not used
#' @return A corpus object with the documents defined as the new units,
#'   including document-level meta-data identifying the original documents.
#' @examples
#' # simple example
#' mycorpus <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                      textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                    docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
#'                    metacorpus = list(notes = "Example showing how corpus_reshape() works."))
#' summary(mycorpus)
#' summary(corpus_reshape(mycorpus, to = "sentences"), showmeta=TRUE)
#' 
#' # example with inaugural corpus speeches
#' (mycorpus2 <- corpus_subset(data_corpus_inaugural, Year>2004))
#' paragCorpus <- corpus_reshape(mycorpus2, to="paragraphs")
#' paragCorpus
#' summary(paragCorpus, 100, showmeta=TRUE)
#' ## Note that Bush 2005 is recorded as a single paragraph because that text used a single
#' ## \n to mark the end of a paragraph.
#' @export
#' @keywords corpus
corpus_reshape <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    UseMethod("corpus_reshape")
}
    
#' @noRd
#' @rdname corpus_reshape
#' @export
corpus_reshape.corpus <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    
    to <- match.arg(to)
    
    if (to == "documents") {
        if (settings(x, 'units') %in% c('sentences', 'paragraphs')) {
            
            docid <- docvars(x, '_docid')
            segid <- docvars(x, '_segid')
            
            if (settings(x, 'units') == 'sentences') {
                texts <- stringi::stri_join_list(split(texts(x), factor(docid)), sep = "  ")
            } else {
                texts <- stringi::stri_join_list(split(texts(x), factor(docid)), sep = "\n\n")
            }

            temp <- corpus(texts, 
                           docnames = docvars(x, '_document')[!duplicated(docid)],
                           docvars = docvars(x)[!duplicated(docid),,drop = FALSE])

            settings(temp, 'units') <- "documents"
            result <- temp
        } else {
            stop("reshape to documents only goes from sentences or paragraphs")
        }
    } else if (to %in% c("sentences", "paragraphs")) {
        if (settings(x, 'units') == 'documents') {
            result <- corpus_segment(x, what = to, ...)
        } else {
            stop("reshape to sentences or paragraphs only goes from documents")
        }
    } else {
        stop("reshape to", to, "is not supported")
    }
    return (result)
}


#' deprecated name for corpus_reshape
#' 
#' The deprecated function name for what is now \code{\link{corpus_reshape}}. 
#' Please use that instead.
#' @param ... all arguments
#' @keywords internal deprecated
#' @export
changeunits <- function(...) {
    .Deprecated("corpus_reshape")
    UseMethod("corpus_reshape")
}


# helper function: rep method for a repeat a data.frame
# Example:
# rep(data.frame(one = 1:2, two = c("a", "b")), 2)
# ## $one
# ## [1] 1 2
# ## 
# ## $two
# ## [1] a b
# ## Levels: a b
# ## 
# ## $one
# ## 
# ## [1] 1 2
# ## $two
# ## [1] a b
# ## Levels: a b
rep.data.frame <- function(x, ...)
    as.data.frame(lapply(x, rep, ...))
