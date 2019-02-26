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
#' corp1 <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                  textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                  docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
#'                  metacorpus = list(notes = "Example showing how corpus_reshape() works."))
#' summary(corp1)
#' summary(corpus_reshape(corp1, to = "sentences"), showmeta = TRUE)
#' 
#' # example with inaugural corpus speeches
#' (corp2 <- corpus_subset(data_corpus_inaugural, Year>2004))
#' corp2para <- corpus_reshape(corp2, to = "paragraphs")
#' corp2para
#' summary(corp2para, 50, showmeta = TRUE)
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
    
    to <- match.arg(to)
    
    if (to == "documents") {
        if (settings(x, 'units') %in% c('sentences', 'paragraphs')) {
            
            docid <- docvars(x, '_docid')
            segid <- docvars(x, '_segid')
            
            if (settings(x, 'units') == 'sentences') {
                texts <- stri_join_list(split(texts(x), factor(docid)), sep = "  ")
            } else {
                texts <- stri_join_list(split(texts(x), factor(docid)), sep = "\n\n")
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
            vars <- docvars(x)
            
            # get the relevant function call
            commands <- as.character(sys.calls())
            commands <- commands[stri_detect_regex(commands, "reshape\\.corpus")]
            
            temp <- segment_texts(texts(x), pattern = NULL, extract_pattern = FALSE, 
                                  omit_empty = FALSE, what = to, ...)
            
            result <- corpus(temp$texts, docnames = rownames(temp),
                             metacorpus = list(source = metacorpus(x, "source"),
                                               notes = commands))
            
            # add repeated versions of remaining docvars
            if (use_docvars && !is.null(vars)) {
                rownames(vars) <- NULL # faster to repeat rows without rownames
                vars <- select_fields(vars, "user")[temp$docid,,drop = FALSE]
                rownames(vars) <- rownames(temp)
                docvars(result) <- vars
            }
            docvars(result, '_document') <- temp$docname
            docvars(result, '_docid') <- temp$docid
            docvars(result, '_segid') <- temp$segid
            settings(result, "units") <- to
        } else {
            stop("reshape to sentences or paragraphs only goes from documents")
        }
        
    } 
    return (result)
}
