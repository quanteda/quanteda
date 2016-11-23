#' change the document units of a corpus
#' 
#' For a corpus, recast the documents down or up a level of aggregation.  "Down"
#' would mean going from documents to sentences, for instance.  "Up" means from 
#' sentences back to documents.  This makes it easy to reshape a corpus from a 
#' collection of documents into a collection of sentences, for instance.
#' (Because the corpus object records its current "units" status, there is no 
#' \code{from} option, only \code{to}.)
#' @param x corpus whose document units will be reshaped
#' @param to new documents units for the corpus to be recast in
#' @param ... not used
#' @return A corpus object with the documents defined as the new units,
#'   including document-level meta-data identifying the original documents.
#' @examples
#' # simple example
#' mycorpus <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                      textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                    docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
#'                    notes = "This is a simple example to show how corpus_reshape() works.")
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
corpus_reshape <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    
    if (as.character(match.call()[[1]]) == "changeunits")
        .Deprecated("corpus_reshape")
    
    to <- match.arg(to)
    if (to == "documents") stop("documents not yet implemented.")
    
    if (length(addedArgs <- names(list(...))))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    # make the new corpus
    segmentedTexts <- char_segment(texts(x), to)
    lengthSegments <- sapply(segmentedTexts, length)
    newcorpus <- corpus(unlist(segmentedTexts))
    # repeat the docvars and existing document metadata
    docvars(newcorpus, names(docvars(x))) <- as.data.frame(lapply(docvars(x), rep, lengthSegments))
    docvars(newcorpus, names(metadoc(x))) <- as.data.frame(lapply(metadoc(x), rep, lengthSegments))
    # add original document name as metadata
    metadoc(newcorpus, "document") <- rep(names(segmentedTexts), lengthSegments)
    # give a serial number (within document) to each sentence
    sentenceid <- lapply(lengthSegments, function(n) seq(from=1, to=n))
    metadoc(newcorpus, "serialno") <- unlist(sentenceid, use.names=FALSE)
    
    # copy settings and corpus metadata
    newcorpus$settings <- x$settings
    newcorpus$metadata <- x$metadata
    
    # modify settings flag for corpus_reshape info
    settings(newcorpus, "unitsoriginal") <- settings(newcorpus, "units")
    settings(newcorpus, "units") <- to
    
    newcorpus
}


#' deprecated name for corpus_reshape
#' 
#' The deprecated function name for what is now \code{\link{corpus_reshape}}. 
#' Please use that instead.
#' @keywords internal
#' @export
changeunits <- corpus_reshape


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
