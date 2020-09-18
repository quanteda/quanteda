#' A paragraph of text for testing various text-based functions
#' 
#' This is a long paragraph (2,914 characters) of text taken from a debate on
#' Joe Higgins, delivered December 8, 2011.
#' @format character vector with one element
#' @keywords data
#' @source Dáil Éireann Debate, 
#' [Financial Resolution No. 13: General (Resumed).](http://oireachtasdebates.oireachtas.ie/debates\%20authoring/debateswebpack.nsf/takes/dail2011120700006?opendocument)
#' 7 December 2011.  vol. 749, no. 1.
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
#' @details `data_corpus_inaugural` is the [quanteda-package] corpus 
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
#' <https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/inaugural-addresses>.
#' @keywords data
"data_corpus_inaugural"

#' Lexicoder Sentiment Dictionary (2015)
#' 
#' The 2015 Lexicoder Sentiment Dictionary in \pkg{quanteda} [dictionary]
#' format.  
#' 
#' @details
#' The dictionary consists of 2,858 "negative" sentiment words and 1,709
#' "positive" sentiment words. A further set of 2,860 and 1,721 negations of
#' negative and positive words, respectively, is also included. While many users
#' will find the non-negation sentiment forms of the LSD adequate for sentiment
#' analysis, Young and Soroka (2012) did find a small, but non-negligible
#' increase in performance when accounting for negations. Users wishing to test
#' this or include the negations are encouraged to subtract negated positive
#' words from the count of positive words, and subtract the negated negative
#' words from the negative count.  
#' 
#' Young and Soroka (2012) also suggest the use of a pre-processing script to
#' remove specific cases of some words (i.e., "good bye", or "nobody better",
#' which should not be counted as positive). Pre-processing scripts are
#' available at <http://www.snsoroka.com/data-lexicoder/>.
#' @section License and Conditions: 
#'   The LSD is available for non-commercial academic purposes only. By using
#'   `data_dictionary_LSD2015`, you accept these terms.
#'   
#'   Please cite the references below when using the dictionary.
#' @format 
#' A [dictionary] of four keys containing glob-style [pattern matches][valuetype].
#' \describe{
#' \item{`negative`}{2,858 word patterns indicating negative sentiment}
#' \item{`positive`}{1,709 word patterns indicating positive sentiment}
#' \item{`neg_positive`}{1,721 word patterns indicating a positive word preceded by a negation (used to convey negative sentiment)}
#' \item{`neg_negative`}{2,860 word patterns indicating a negative word preceded by a negation (used to convey positive sentiment)}
#' }
#' @references 
#'   The objectives, development and reliability of the dictionary are discussed
#'   in detail in Young and Soroka (2012). Please cite this article when using the
#'   Lexicoder Sentiment Dictionary and related resources.
#
#'   Young, L. & Soroka, S. (2012). *Lexicoder Sentiment
#'   Dictionary*. Available at <http://www.snsoroka.com/data-lexicoder/>.
#'   
#'   Young, L. & Soroka, S. (2012). 
#'   [Affective News: 
#'   The Automated Coding of Sentiment in Political Texts](https://doi.org/10.1080/10584609.2012.671234). 
#'   *Political Communication*, 29(2), 205--231.
#' @keywords data
#' @examples 
#' # simple example
#' txt <- "This aggressive policy will not win friends."
#' 
#' tokens_lookup(tokens(txt), dictionary = data_dictionary_LSD2015, exclusive = FALSE)
#' ## tokens from 1 document.
#' ## text1 :
#' ## [1] "This"   "NEGATIVE"   "policy"   "will"   "NEG_POSITIVE"   "POSITIVE"   "POSITIVE" "."
#' 
#' # notice that double-counting of negated and non-negated terms is avoided 
#' # when using nested_scope = "dictionary"
#' tokens_lookup(tokens(txt), dictionary = data_dictionary_LSD2015, 
#'               exclusive = FALSE, nested_scope = "dictionary")
#' ## tokens from 1 document.
#' ## text1 :
#' ## [1] "This"   "NEGATIVE"   "policy"   "will"   "NEG_POSITIVE" "POSITIVE."   
#' 
#' # on larger examples - notice that few negations are used
#' dfm(data_char_ukimmig2010[1:5], dictionary = data_dictionary_LSD2015)
#' 
#' # compound neg_negative and neg_positive tokens before creating a dfm object
#' toks <- tokens_compound(tokens(txt), data_dictionary_LSD2015)
#' 
#' dfm_lookup(dfm(toks), data_dictionary_LSD2015)
"data_dictionary_LSD2015"

#' Formerly included data objects
#' 
#' The following corpus objects have been relocated to the \pkg{quanteda.textmodels} 
#' package:
#' * `data_corpus_dailnoconf1991`
#' * `data_corpus_irishbudget2010`
#' @name data-relocated
#' @aliases data_corpus_dailnoconf1991 data_corpus_irishbudget2010
#' @seealso `quanteda.textmodels::quanteda.textmodels-package`
NULL

#' Internal data sets
#' 
#' Data sets used for mainly internal purposes by the \pkg{quanteda} package.
#' @name data-internal
#' @docType data
#' @keywords data internal
NULL

