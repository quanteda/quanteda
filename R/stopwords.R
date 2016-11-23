#' @rdname data-internal
#' @details
#' \code{data_char_stopwords} provides stopword lists in multiple languages; 
#' it is a named list of characters with the lowercase language name (in English) as the 
#' name of each list element.  The English stopwords are taken from the SMART
#' information retrieval system (obtained from 
#' \url{http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop})
#' and a set of stopword lists from the Snowball stemmer project in different 
#' languages (see \url{http://snowballstem.org/projects.html}). Supported
#' languages are Arabic, Danish, Dutch, English, Finnish, French, German,
#' Hungarian, Italian, Norwegian, Portuguese, Russian, Spanish, and Swedish.
"data_char_stopwords"

#' access built-in stopwords
#' 
#' This function retrieves stopwords from the type specified in the \code{kind} 
#' argument and returns the stopword list as a character vector The default is 
#' English.
#' 
#' The stopword list is an internal data object named
#' \code{\link{data_char_stopwords}}, which consists of English stopwords from the SMART
#' information retrieval system (obtained from 
#' \url{http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop})
#' and a set of stopword lists from the Snowball stemmer project in different 
#' languages (see \url{http://snowballstem.org/projects.html}). Supported
#' languages are Arabic, Danish, Dutch, English, Finnish, French, German,
#' Hungarian, Italian, Norwegian, Portuguese, Russian, Spanish, and Swedish.
#' Language names should be lower-case (except for "SMART" -- see below) and are  
#' case sensitive.
#' @rdname stopwords
#' @section A note of caution: Stop words are an arbitrary choice imposed by the
#'   user, and accessing a pre-defined list of words to ignore does not mean 
#'   that it will perfectly fit your needs. You are strongly encourged to 
#'   inspect the list and to make sure it fits your particular requirements. 
#'   The built-in English stopword list does not contain "will", for instance,
#'   because of its multiple meanings, but you might want to include this word
#'   for your own application.
#' @param kind The pre-set kind of stopwords (as a character string).  Allowed 
#'   values are \code{english}, \code{SMART}, \code{danish}, \code{french}, 
#'   \code{hungarian}, \code{norwegian}, \code{russian}, \code{swedish}, 
#'   \code{catalan}, \code{dutch}, \code{finnish}, \code{german}, 
#'   \code{italian}, \code{portuguese}, \code{spanish}, \code{arabic}
#' @return a character vector of stopwords
#' @name stopwords
#' @export
#' @examples
#' head(stopwords("english"))
#' head(stopwords("italian"))
#' head(stopwords("arabic"))
#' head(stopwords("SMART"))
#' 
#' # adding to the built-in stopword list
#' toks <- tokenize("The judge will sentence Mr. Adams to nine years in prison", removePunct = TRUE)
#' removeFeatures(toks, c(stopwords("english"), "will", "mr", "nine"))
stopwords <- function(kind = "english") {
    if (!(kind %in% names(quanteda::data_char_stopwords)))
        stop(paste0("\"", kind, "\" is not a recognized stopword list name."))
    quanteda::data_char_stopwords[[kind]]
}


