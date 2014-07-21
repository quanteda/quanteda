#' @name amicusCorpus
#' @title Amicus briefs from  US Supreme court cases
#' @description This is a corpus of petitioner and respondent briefs to two US Supreme court cases on affirmative action; Bakke(1978) and Bollinger (2008)
#' @format corpus with 100 texts, and attribute for petitioner/respondent
#' @references Evans, Michael, et al. "Recounting the courts? Applying automated content analysis to enhance empirical legal research." Journal of Empirical Legal Studies 4.4 (2007): 1007-1039.
#' @docType data
NULL

#' @name amicusTexts
#' @title Amicus briefs from  US Supreme court cases
#' @description This is a text vector of petitioner and respondent briefs to two US Supreme court cases on affirmative action; Bakke(1978) and Bollinger (2008)
#' @format character vector with 100 texts
#' @references Evans, Michael, et al. "Recounting the courts? Applying automated content analysis to enhance empirical legal research." Journal of Empirical Legal Studies 4.4 (2007): 1007-1039.
#' @docType data
NULL


#' @name exampleString
#' @title exampleString for testing functions
#' @description This is a long paragraph (2914 characters) of text taken from an Irish budget speech by Joe Higgins
#' @format character vector with one element (nchar 2914)
#' @docType data
NULL


#' @name ieAttribs
#' @title A vector of attributes to match ieBudget documents
#' @description This is a small vector of attributes for use in examples with ieBudgets
#' @format a dataframe of attributes, 5 obs. of 6 variables 
#' @docType data
NULL


#' @name ieTextsHeaders
#' @title Irish budget speeches headers
#' @description This is a small vector of texts for use in examples with corpusFromHeaders
#' @format character vector of 14 texts with JSON headers for use in example
#' @docType data
NULL

#' @name ieTexts
#' @title Irish budget speeches texts
#' @description This is a small vector of texts from the ieBudget corpus for use with testing examples
#' @format character vector of 5 texts
#' @docType data
NULL

#' @name iebudgets
#' @title Irish budget speeches corpus
#' @description A corpus containing speeches from Irish budget debates in 2008-2012. Each text has attributes
#' for party, speaker and year 
#' @references \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2225069}
#' @docType data
NULL

#' @name movies
#' @title A corpus object containing 2000 movie reviews
#' @docType data
#' @description A corpus object containing 2000 movie reviews classified by positive or negative sentiment
#' @references \url{http://dl.acm.org/citation.cfm?id=1118704}
NULL


#' @name UKManifestos
#' @title A corpus object containing 105 UK Manifestos
#' @docType data
#' @description A corpus object containing 105 UK Manifestos from 1945-2005, with party and year attributes
#' @references As used in Laver, Michael. 1998a. \'Party Policy in Britain, 1997: Results from an Expert Survey.\' Political Studies 46: 336–47.
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
#' data(sylCounts)
#' syllableCounts["sixths"]
#' syllableCounts["onomatopeia"]
#' @references \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}
#' 
NULL
