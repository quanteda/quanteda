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
#' @title Immigration-related sections of 2010 UK party manifestos
#' @docType data
#' @description Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @examples
#' data(uk2010immig)
#' uk2010immigCorpus <- corpus(uk2010immig, docvars=list(party=names(uk2010immig)))
#' language(uk2010immigCorpus) <- "english"
#' encoding(uk2010immigCorpus) <- "UTF-8"
#' summary(uk2010immigCorpus)
NULL


#' @name stopwords
#' @title A named list containing common stopwords in 14 languages
# @rdname stopwordsRemove
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

