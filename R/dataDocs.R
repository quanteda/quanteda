
#' @name inaugCorpus
#' @docType data
#' @title A corpus of US presidential inaugural addresses from 1789-2013
#' @description \code{inaugCorpus} is the \link{quanteda-package} corpus object of US presidents' inaugural addresses since 1789.
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


#' @name englishSyllables
#' @docType data
#' @title syllable counts of English words
#' @description A named numeric vector of syllable counts for the words used as names.
#' Taken from the freely available CMU pronunciation dictionary.
#' @examples
#' data(englishSyllables)
#' englishSyllables["sixths"]
#' englishSyllables["onomatopeia"]
#' @source \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}
#' 
NULL

#' @name .stopwords
#' @rdname stopwords
#' @docType data
NULL

