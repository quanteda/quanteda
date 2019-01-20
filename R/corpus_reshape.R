#' Recast the document units of a corpus
#' 
#' For a corpus, reshape (or recast) the documents to a different level of aggregation.  
#' Units of aggregation can be defined as documents, paragraphs, or sentences.
#' Because the corpus object records its current "units" status, it is possible
#' to move from recast units back to original units, for example from documents,
#' to sentences, and then back to documents (possibly after modifying the sentences).
#' @param x corpus whose document units will be reshaped
#' @param to new document units in which the corpus will be recast
#' @param ... additional arguments passed to \code{\link{tokens}}, since the
#'   syntactic segmenter uses this function)
#' @inheritParams corpus_segment
#' @return A corpus object with the documents defined as the new units,
#'   including document-level meta-data identifying the original documents.
#' @examples
#' # simple example
#' corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                  textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                  docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)))
#' summary(corp)
#' summary(corpus_reshape(corp, to = "sentences"))
#' 
#' # example with inaugural corpus speeches
#' (corp2 <- corpus_subset(data_corpus_inaugural, Year>2004))
#' corp2_para <- corpus_reshape(corp2, to = "paragraphs")
#' corp2_para
#' summary(corp2_para, 50, showmeta = TRUE)
#' ## Note that Bush 2005 is recorded as a single paragraph because that text 
#' ## used a single \n to mark the end of a paragraph.
#' @export
#' @import stringi
#' @keywords corpus
corpus_reshape <- function(x, to = c("sentences", "paragraphs", "documents"), 
                           use_docvars = TRUE, ...) {
    UseMethod("corpus_reshape")
}
    
#' @export
corpus_reshape.default <- function(x, to = c("sentences", "paragraphs", "documents"), 
                                   use_docvars = TRUE, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus_reshape"))
}

#' @export
corpus_reshape.corpus <- function(x, to = c("sentences", "paragraphs", "documents"), 
                                  use_docvars = TRUE, ...) {
    
    x <- as.corpus(x)
    to <- match.arg(to)
    attrs <- attributes(x)
    docvar <- attr(x, "docvars")
    
    if (to == "documents") {
        if (attr(x, 'unit') %in% c('sentences', 'paragraphs')) {
            temp <- split(unclass(x), droplevels(docvar[["docid_"]]))
            if (identical(attrs$unit, "sentences")) {
                temp <- unlist(lapply(temp, paste0, collapse = "  "))
            } else {
                temp <- unlist(lapply(temp, paste0, collapse = "\n\n"))
            }
            docvar <- reshape_docvars(docvar, !duplicated(docvar[["docid_"]]))
            result <- corpus(temp, docvars = select_docvars(docvar))
            attr(result, "unit") <- "documents"
        } else {
            stop("reshape to documents only goes from sentences or paragraphs")
        }
    } else if (to %in% c("sentences", "paragraphs")) {
        if (identical(attrs$unit, "documents")) {
            temp <- segment_texts(x,  pattern = NULL, extract_pattern = FALSE, 
                                  omit_empty = FALSE, what = to, ...)
            docvar <- reshape_docvars(docvar, temp$docnum)
            if (use_docvars) {
                result <- corpus(temp$text, docvar[["docid_"]], select_docvars(docvar))
            } else {
                result <- corpus(temp$text, docvar[["docid_"]])
            }
            attr(result, "unit") <- to
        } else {
            stop("reshape to sentences or paragraphs only goes from documents")
        }
    } 
    return (result)
}
