#' @name ieAttribs
#' @title A vector of attributes to match ieBudget documents
#' @description This is a small vector of attributes for use in examples with ieBudgets
#' @docType data
NULL

#' @name ieTextsHeaders
#' @title Irish budget speeches headers
#' @description This is a small vector of texts for use in examples with corpusFromHeaders
#' @docType data
NULL

#' @name ieTexts
#' @title Irish budget speeches texts
#' @description This is a small vector of texts from the ieBudget corpus for use with testing examples
#' @docType data
NULL

#' @name iebudgets
#' @title Irish budget speeches corpus
#' @description A corpus containing speeches from Irish budget debates in 2008-2012. Each text has attributes
#' for party, speaker and year 
#' @docType data
NULL

#' @name movies
#' @title A corpus object containing 2000 movie reviews
#' @docType data
#' @description A corpus object containing 2000 movie reviews classified by positive or negative sentiment
#' @references \url{http://dl.acm.org/citation.cfm?id=1118704}
NULL

#' @name stopwords
#' @title A named list containing common stopwords in 14 languages
#' @docType data
#' @description SMART English stopwords from the SMART information retrieval
#' system (obtained from
#' http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop)
#' and a set of stopword lists from the Snowball stemmer project in
#' different languages (obtained from
#' http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt). Supported
#' languages are danish, dutch, english, finnish, french, german,
#' hungarian, italian, norwegian, portuguese, russian, spanish, and
#' swedish. Language names are case sensitive. Alternatively, their IETF
#' language tags may be used.
NULL

#' @name syllableCounts
#' @docType data
#' @title A named list mapping words to counts of their syllables
#' @description A named list mapping words to counts of their syllables, generated from
#' the CMU pronunciation dictionary
#' @examples
#' data(sylcounts)
#' syllableCounts["sixths"]
#' syllableCounts["onomatopeia"]
#' @references \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}
#' 
NULL
