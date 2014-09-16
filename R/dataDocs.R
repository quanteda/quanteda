#' @name quanteda
#' @docType package
#' @title An R package for the quantitative analysis of textual data.
#' @author Ken Benoit and Paul Nulty
#' @description A set of functions for creating and managing text corpora, extracting features from 
#' text corpora, and analyzing those features using quantitative methods.
NULL

#' @name inaugCorpus
#' @docType data
#' @title A corpus of US presidential inaugural addresses from 1789-2013
#' @description \code{inaugCorpus} is the \link{quanteda} corpus object of US presidents' inaugural addresses since 1789.
#' Document variables contain the year of the address and the last name of the president.
#' @examples
#' # some operations on the inaugural corpus
#' data(inaugCorpus)
#' summary(inaugCorpus)
#' head(docvars(inaugCorpus), 10)
#' @references \url{https://archive.org/details/Inaugural-Address-Corpus-1789-2009} and 
#' \url{http://www.presidency.ucsb.edu/inaugurals.php}.
#' 
NULL

#' @name inaugTexts
#' @title Texts of US presidential inaugural addresses from 1789-2013
#' @rdname inaugCorpus
#' @description \code{inaugTexts} is the character vector of US presidential inaugaration speeches
#' @docType data
#' @examples
#' # working with the character vector only
#' data(inaugTexts)
#' str(inaugTexts)
#' head(docvars(inaugCorpus), 10)
#' mycorpus <- corpus(inaugTexts)
NULL


#' @name uk2010immig
#' @description Character vector of immigration-related sections from UK 2010 political party manifestos.
#' @title Texts of UK 2010 manifestos on immigration
#' @docType data
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
#' data(syllableCounts)
#' syllableCounts["sixths"]
#' syllableCounts["onomatopeia"]
#' @references \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}
#' 
NULL

