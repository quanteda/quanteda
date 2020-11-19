#' A paragraph of text for testing various text-based functions
#'
#' This is a long paragraph (2,914 characters) of text taken from a debate on
#' Joe Higgins, delivered December 8, 2011.
#' @format character vector with one element
#' @keywords data
#' @source Dáil Éireann Debate, [Financial Resolution No. 13: General
#'   (Resumed).](http://oireachtasdebates.oireachtas.ie/debates\%20authoring/debateswebpack.nsf/takes/dail2011120700006?opendocument)
#'    7 December 2011.  vol. 749, no. 1.
#' @examples
#' tokens(data_char_sampletext, remove_punct = TRUE)
"data_char_sampletext"

#' Immigration-related sections of 2010 UK party manifestos
#'
#' Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @keywords data
#' @examples
#' data_corpus_ukimmig2010 <-
#'     corpus(data_char_ukimmig2010,
#'            docvars = data.frame(party = names(data_char_ukimmig2010)))
#' summary(data_corpus_ukimmig2010, showmeta = TRUE)
"data_char_ukimmig2010"

#' dfm from data in Table 1 of Laver, Benoit, and Garry (2003)
#'
#' Constructed example data to demonstrate the Wordscores algorithm, from Laver
#' Benoit and Garry (2003), Table 1.
#' @details This is the example word count data from Laver, Benoit and Garry's
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions:
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @keywords data
#' @aliases data_dfm_LBGexample
#' @format A [dfm] object with 6 documents and 37 features.
#' @references Laver, M., Benoit, K.R., & Garry, J. (2003).
#'   [Estimating Policy
#'   Positions from Political Text using Words as Data](https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf). *American
#'   Political Science Review*, 97(2), 311--331.
"data_dfm_lbgexample"


#' US presidential inaugural address texts
#'
#' US presidential inaugural address texts, and metadata (for the corpus), from
#' 1789 to present.
#' @details `data_corpus_inaugural` is the corpus
#'   object of US presidents' inaugural addresses since 1789. Document variables
#'   contain the year of the address and the last name of the president.
#' @format a [corpus] object with the following docvars:
#' * `Year` a four-digit integer year
#' * `President` character; President's last name
#' * `FirstName` character; President's first name (and possibly middle initial)
#' @examples
#' # some operations on the inaugural corpus
#' summary(data_corpus_inaugural)
#' head(docvars(data_corpus_inaugural), 10)
#' @source
#' <https://archive.org/details/Inaugural-Address-Corpus-1789-2009> and
#' <http://www.presidency.ucsb.edu/inaugurals.php>.
#' @keywords data
"data_corpus_inaugural"


#' Formerly included data objects
#'
#' The following corpus objects have been relocated to other (quanteda)
#' packages.
#' package:
#' * `data_corpus_dailnoconf1991`
#' * `data_corpus_irishbudget2010`
#' * `data_dictionary_LSD2015`
#' @name data-relocated
#' @aliases data_corpus_dailnoconf1991 data_corpus_irishbudget2010 data_dictionary_LSD2015
#' @seealso `quanteda.textmodels::quanteda.textmodels-package`,
#'   `quanteda.sentiment::quanteda.sentiment-package`
NULL

#' Internal data sets
#'
#' Data sets used for mainly internal purposes by the \pkg{quanteda} package.
#' @name data-internal
#' @docType data
#' @keywords data internal
NULL

