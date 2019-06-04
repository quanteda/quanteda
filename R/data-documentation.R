#' A paragraph of text for testing various text-based functions
#' 
#' This is a long paragraph (2,914 characters) of text taken from a debate on
#' Joe Higgins, delivered December 8, 2011.
#' @format character vector with one element
#' @keywords data
#' @source Dáil Éireann Debate, 
#' \href{http://oireachtasdebates.oireachtas.ie/debates\%20authoring/debateswebpack.nsf/takes/dail2011120700006?opendocument}{Financial Resolution No. 13: General (Resumed).}
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
#' metadoc(data_corpus_ukimmig2010, "language") <- "english"
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
#' @format A \link{dfm} object with 6 documents and 37 features.
#' @references Laver, M., Benoit, K.R., & Garry, J. (2003). 
#'   \href{https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating Policy 
#'   Positions from Political Text using Words as Data}. \emph{American
#'   Political Science Review}, 97(2), 311--331.
"data_dfm_lbgexample"  

#' Irish budget speeches from 2010
#' 
#' Speeches and document-level variables from the debate over the Irish budget
#' of 2010.
#' @details At the time of the debate, Fianna Fáil (FF) and the Greens formed the government
#'   coalition, while Fine Gael (FG), Labour (LAB), and Sinn Féin (SF) were in opposition.
#' @format The corpus object for the 2010 budget speeches, with document-level 
#'   variables for year, debate, serial number, first and last name of the 
#'   speaker, and the speaker's party.
#' @references Lowe, W. & Benoit, K.R. (2013). 
#'   \href{https://doi.org/10.1093/pan/mpt002}{Validating Estimates of Latent Traits 
#'   From Textual Data Using Human Judgment as a Benchmark}.
#'   \emph{Political Analysis}, 21(3), 298--313.
#' @keywords data
#' @source 
#' Dáil Éireann Debate, 
#' \href{http://oireachtasdebates.oireachtas.ie/debates\%20authoring/debateswebpack.nsf/takes/dail2009120900022?opendocument}{Budget Statement 2010.}
#' 9 December 2009. vol. 697, no. 3.
#' 
#' @examples
#' summary(data_corpus_irishbudget2010)
"data_corpus_irishbudget2010"

# data(data_corpus_irishbudget2010, package="quantedaData")
# txts <- texts(data_corpus_irishbudget2010)
# for (i in seq_along(txts)) {
#     temptxt <- system2("native2ascii", input = txts[i], stdout = TRUE)
#     temptxt[temptxt==""] <- "\n"
#     temptxt <- paste(temptxt, collapse="")
#     txts[i] <- temptxt
# }
# texts(data_corpus_irishbudget2010) <- txts
# save(data_corpus_irishbudget2010, file = "data/data_corpus_irishbudget2010.RData")


#' Internal data sets
#' 
#' Data sets used for mainly internal purposes by the \pkg{quanteda} package.
#' @name data-internal
#' @docType data
#' @keywords data internal
NULL


#' US presidential inaugural address texts
#' 
#' US presidential inaugural address texts, and metadata (for the corpus), from
#' 1789 to present.
#' @details \code{data_corpus_inaugural} is the \link{quanteda-package} corpus 
#'   object of US presidents' inaugural addresses since 1789. Document variables
#'   contain the year of the address and the last name of the president.
#' @format a \link{corpus} object with the following docvars:
#' \itemize{
#' \item \code{Year} a four-digit integer year
#' \item \code{President} character; President's last name
#' \item \code{FirstName} character; President's first name (and possibly middle initial)
#' }
#' @examples
#' # some operations on the inaugural corpus
#' summary(data_corpus_inaugural)
#' head(docvars(data_corpus_inaugural), 10)
#' @source 
#' \url{https://archive.org/details/Inaugural-Address-Corpus-1789-2009} and 
#' \url{http://www.presidency.ucsb.edu/inaugurals.php}.
#' @keywords data
"data_corpus_inaugural"

#' Lexicoder Sentiment Dictionary (2015)
#' 
#' The 2015 Lexicoder Sentiment Dictionary in \pkg{quanteda} \link{dictionary}
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
#' available at \url{http://lexicoder.com}.
#' @section License and Conditions: 
#'   The LSD is available for non-commercial academic purposes only. By using
#'   \code{data_dictionary_LSD2015}, you accept these terms.
#'   
#'   Please cite the references below when using the dictionary.
#' @format 
#' A \link{dictionary} of four keys containing glob-style \link[=valuetype]{pattern matches}.
#' \describe{
#' \item{\code{negative}}{2,858 word patterns indicating negative sentiment}
#' \item{\code{positive}}{1,709 word patterns indicating positive sentiment}
#' \item{\code{neg_positive}}{1,721 word patterns indicating a positive word preceded by a negation (used to convey negative sentiment)}
#' \item{\code{neg_negative}}{2,860 word patterns indicating a negative word preceded by a negation (used to convey positive sentiment)}
#' }
#' @references 
#'   The objectives, development and reliability of the dictionary are discussed
#'   in detail in Young and Soroka (2012). Please cite this article when using the
#'   Lexicoder Sentiment Dictionary and related resources.
#
#'   Young, L. & Soroka, S. (2012). \emph{Lexicoder Sentiment
#'   Dictionary}. Available at \url{http://lexicoder.com}.
#'   
#'   Young, L. & Soroka, S. (2012). 
#'   \href{https://doi.org/10.1080/10584609.2012.671234}{Affective News: 
#'   The Automated Coding of Sentiment in Political Texts}. 
#'   \emph{Political Communication}, 29(2), 205--231.
#' @keywords data
#' @examples 
#' # simple example
#' txt <- "This aggressive policy will not win friends."
#' tokens_lookup(tokens(txt), dictionary = data_dictionary_LSD2015, exclusive = FALSE)
#' ## tokens from 1 document.
#' ## text1 :
#' ## [1] "This"   "NEGATIVE"   "policy"   "will"   "NEG_POSITIVE" "POSITIVE" "."
#' 
#' # on larger examples - notice that few negations are used
#' dfm(data_char_ukimmig2010, dictionary = data_dictionary_LSD2015)
#' kwic(data_char_ukimmig2010, "not")
#' 
#' # compound neg_negative and neg_positive tokens before creating a dfm object
#' toks <- tokens_compound(tokens(txt), data_dictionary_LSD2015)
#' 
#' dfm_lookup(dfm(toks), data_dictionary_LSD2015)
"data_dictionary_LSD2015"

#' Confidence debate from 1991 Irish Parliament
#' 
#' Texts of speeches from a no-confidence motion debated in the Irish Dáil from
#' 16-18 October 1991 over the future of the Fianna Fail-Progressive Democrat
#' coalition.  (See Laver and Benoit 2002 for details.)
#' @format \code{data_corpus_dailnoconf1991} is a corpus with 58 texts, 
#'   including docvars for \code{name}, \code{party}, and \code{position}.
#' @references Laver, M. & Benoit, K.R. (2002). 
#'   \href{https://kenbenoit.net/pdfs/Laver_Benoit_IPS_2002.pdf}{Locating 
#'   TDs in Policy Spaces: Wordscoring Dáil Speeches}. \emph{Irish Political 
#'   Studies}, 17(1), 59--73.
#'   
#' @references Laver, M., Benoit, K.R., & Garry, J. (2003). 
#'   \href{https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating Policy 
#'   Positions from Political Text using Words as Data}. \emph{American
#'   Political Science Review}, 97(2), 311--331.
#' @source \url{https://www.oireachtas.ie/en/debates/debate/dail/1991-10-16/10/}
#' @keywords data
#' @examples
#' \dontrun{
#' data_dfm_dailnoconf1991 <- dfm(data_corpus_dailnoconf1991, remove_punct = TRUE)
#' tmod <- textmodel_affinity(data_dfm_dailnoconf1991, 
#'                            c("Govt", "Opp", "Opp", rep(NA, 55)))
#' (pred <- predict(tmod))
#' dat <- 
#'     data.frame(party = as.character(docvars(data_corpus_dailnoconf1991, "party")),
#'                govt = coef(pred)[, "Govt"],
#'                position = as.character(docvars(data_corpus_dailnoconf1991, "position")),
#'                stringsAsFactors = FALSE)
#' bymedian <- with(dat, reorder(paste(party, position), govt, median))
#' par(mar = c(5, 6, 4, 2)+.1)
#' boxplot(govt ~ bymedian, data = dat,
#'         horizontal = TRUE, las = 1,
#'         xlab = "Degree of support for government")
#' abline(h = 7.5, col = "red", lty = "dashed")
#' text(c(0.9, 0.9), c(8.5, 6.5), c("Goverment", "Opposition"))
#' }
"data_corpus_dailnoconf1991"
