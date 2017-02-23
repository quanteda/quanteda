#' change the document units of a corpus
#' 
#' For a corpus, recast the documents down or up a level of aggregation.  "Down"
#' would mean going from documents to sentences, for instance.  "Up" means from 
#' sentences back to documents.  This makes it easy to reshape a corpus from a 
#' collection of documents into a collection of sentences, for instance.
#' (Because the corpus object records its current "units" status, there is no 
#' \code{from} option, only \code{to}.)
#' 
#' Note: Only recasting down currently works, but upward recasting is planned.
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
#' @importFrom data.table data.table setnames
#' @export
corpus_reshape.corpus <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    
    if (as.character(match.call()[[1]]) == "changeunits")
        .Deprecated("corpus_reshape")
    
    to <- match.arg(to)
    
    if (length(addedArgs <- names(list(...))))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (to == "documents") {
        if (settings(x, "unitsoriginal") != "documents" & !(settings(x, "units") %in% c("sentences")))
            stop("reshape to documents only goes from sentences to documents")
        
        if (settings(x, "units") == "paragraphs") {
            spacer <- "\n\n"
        } else {
            spacer <- "  "
        }
        
        # reshape into original documents, replace the original text
        docs <- data.table(x$documents)
        setnames(docs, "_document", "document")
        # take just first value of every (repeated) docvar
        docs <- docs[, lapply(.SD, function(x) x[1]), by = document]
        # concatenate texts
        docs[, texts := texts(x, groups = metadoc(x, "document")[, 1], spacer = spacer)]
        
        # make the text "empty" if it contains only spaces
        docs[stringi::stri_detect_regex(texts, "^\\s+$"), texts := ""]
        
        # remove reshape fields
        docs[, "_serialno" := NULL]
        
        newcorpus <- x
        newcorpus$documents <- as.data.frame(docs[, -which(names(docs) == "document"), with = FALSE])
        rownames(newcorpus$documents) <- docs$document
    
    } else {
        
        # make the new corpus
        segmentedTexts <- lapply(texts(x), char_segment, what = to)
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
    
    }
    
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
