
# #' @param corp a quanteda corpus object
# #' @rdname print
# #' @export
# #' @examples
# #' inaugCorpus  # inaugural address corpus
# #' @export
# print.corpus2 <- function(corp) {
#     cat("Corpus consisting of", length(corp), "documents.\n")
# }


#' Corpus-level metadata
#' 
#' Get or set the corpus-level metadata in a quanteda corpus object.
#' 
#' @param corp A quanteda corpus object
#' @param fields Metadata field names.  If NULL (default), return all metadata names.
#' @return For \code{metacorpus}, a list of the metadata fields in the corpus.
#' 
#' For \code{metacorpus <-}, the corpus with the updated metadata.
#' @export
#' @examples
#' metacorpus(inaugTexts)
#' metacorpus(inaugTexts, "source")
#' metacorpus(inaugTexts, "citation") <- "Presidential Speeches Online Project (2014)."
metacorpus <- function(corp, fields=NULL) {
    if (!is.null(fields)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
        return(corp$metadata[fields])
    } else {
        return(corp$metadata)
    }
}

# replacement function for corpus-level data
#' @export
"metacorpus<-" <- function(corp, value, fields) {
    if (!is.null(fields)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    corp$metadata[fields] <- value
    corp
}

settings <- list(stopwords=NULL,    # NULL, or stopword list
                 collocations=NULL, # NULL, or collocations object
                 dictionary=NULL,   # NULL, or dictionary object
                 stem=NULL,         # NULL, or language name
                 delimiters_sentence = __DEFAULT_DELIM_SENTENCE,
                 delimiters_word = __DEFAULT_DELIM_WORD,
                 delimiters_paragraph = __DEFAULT_DELIM_PARAGRAPH,
                 clean = list(options="options here")
                 # these options will correspond the clean() arguments
                 )

settings(corpus, "setting")<- will assign corpus level settings
settings list: (defaults are NULL)
stopwords
collocations
dictionary
stemming (settings: none, English, etc (from SnowBall))
delimiters_sentence.  ".!?"
delimiters_word - default " ", but could be c(" ", ";", "•")
delimiters_paragraph - default \n\n, but could be \n
clean settings
settings(corpus) - prints the current settings

