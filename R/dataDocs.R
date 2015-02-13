
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

#' @name ukimmigrTexts
#' @title Immigration-related sections of 2010 UK party manifestos
#' @docType data
#' @description Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @examples
#' data(ukimmigrTexts)
#' ukimmigrCorpus <- corpus(ukimmigrTexts, docvars=list(party=names(ukimmigrTexts)))
#' language(ukimmigrCorpus) <- "english"
#' encoding(ukimmigrCorpus) <- "UTF-8"
#' summary(ukimmigrCorpus)
NULL

#' @name englishSyllables
#' @rdname syllables
#' @docType data
#' @details \code{englishSyllables} is a quanteda-supplied data object consisting of a named numeric 
#' vector of syllable counts for the words used as names.  This is the default object used to count 
#' English syllables.
# @examples
# data(englishSyllables)
# englishSyllables["sixths"]
# englishSyllables["onomatopeia"]
#' @source \code{englishSyllables} is built from the freely available CMU pronunciation dictionary at \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}.
#' 
NULL

#' @name LBGexample
#' @docType data
#' @title dfm containing example data from Table 1 of Laver Benoit and Garry
#'   (2003)
#' @description Example data to demonstrate the Wordscores algorithm, from Laver
#'   Benoit and Garry (2003)
#' @details This is the example word count data from Laver, Benoit and Garry's
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions:
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @format A \link{dfm} object with 6 documents and 37 features
#' @references Laver, Michael, Kenneth Benoit, and John Garry.  2003.
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating policy
#'   positions from political text using words as data.}" American Political
#'   Science Review 97(2): 311-331.
NULL

# @name ie2010Corpus
# @aliases iebudgets
# @title Irish budget speeches from 2010
# @description Speeches and document-level variables from the debate over the 
#   Irish budget of 2010.
# @format The corpus object for the 2010 budget speeches, with document-level
#   variables for year, debate, serial number, first and last name of the
#   speaker, and the speaker's party.
# @source Lowe and Benoit (2013)
# @references Lowe, Will, and Kenneth R Benoit. 2013. 
#   "\href{http://www.kenbenoit.net/pdfs/Political%20Analysis-2013-Lowe-298-313.pdf}{Validating
#    Estimates of Latent Traits From Textual Data Using Human Judgment as a 
#   Benchmark.}" \emph{Political Analysis} 21: 298–313.
# @docType data summary(ie2010Corpus)
# NULL

