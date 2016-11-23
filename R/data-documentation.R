#' a paragraph of text for testing various text-based functions
#' 
#' This is a long paragraph (2,914 characters) of text taken from an Irish budget speech by Joe Higgins.
#' @format character vector with one element
#' @aliases exampleString
#' @examples
#' tokenize(data_char_sampletext, removePunct = TRUE)
"data_char_sampletext"

#' Project Gutenberg text of Herman Melville's \emph{Moby Dick}
#' Named character object of the ASCII text of Herman Melville's
#'  \emph{Moby Dick}, EBook no. 2701.
#' @source Project Gutenberg, \url{http://www.gutenberg.org}
#' @aliases mobydickText
#' @examples
#' summary(data_char_mobydick)
"data_char_mobydick"

#' Immigration-related sections of 2010 UK party manifestos
#' Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @examples
#' data_corpus_ukimmig2010 <- 
#'     corpus(data_char_ukimmig2010, 
#'            docvars = data.frame(party = names(ukimmigTexts)))
#' metadoc(data_corpus_ukimmig2010, "language") <- "english"
#' summary(data_corpus_ukimmig2010, showmeta = TRUE)
"data_char_ukimmig2010"

#' dfm with example data from Table 1 of Laver Benoit and Garry (2003)
#' 
#' Constructed example data to demonstrate the Wordscores algorithm, from Laver 
#' Benoit and Garry (2003), Table 1.
#' @details This is the example word count data from Laver, Benoit and Garry's 
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions: 
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @format A \link{dfm} object with 6 documents and 37 features
#' @references Laver, Michael, Kenneth Benoit, and John Garry.  2003. 
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating policy 
#'   positions from political text using words as data.}" \emph{American
#'   Political Science Review} 97(2): 311-331.
#' @aliases LBGexample
"data_dfm_LBGexample"  

#' Irish budget speeches from 2010
#' 
#' Speeches and document-level variables from the debate over the Irish budget
#' of 2010.
#' @format The corpus object for the 2010 budget speeches, with document-level 
#'   variables for year, debate, serial number, first and last name of the 
#'   speaker, and the speaker's party.
#' @source Lowe, Will, and Kenneth R Benoit. 2013. "Validating Estimates of 
#'   Latent Traits From Textual Data Using Human Judgment as a Benchmark." 
#'   \emph{Political Analysis} 21: 298-313.
#' @aliases iebudgets ie2010Corpus
#' @examples
#' summary(data_corpus_irishbudget2010)
"data_corpus_irishbudget2010"

# data(data_corpus_irishbudget2010, package="quantedaData")
# txts <- texts(data_corpus_irishbudget2010)
# for (i in 1:length(txts)) {
#     temptxt <- system2("native2ascii", input = txts[i], stdout = TRUE)
#     temptxt[temptxt==""] <- "\n"
#     temptxt <- paste(temptxt, collapse="")
#     txts[i] <- temptxt
# }
# texts(data_corpus_irishbudget2010) <- txts
# save(data_corpus_irishbudget2010, file="data/data_corpus_irishbudget2010.RData")


#' encoded texts for testing
#' 
#' \code{data_char_encodedtexts} is a 10-element character vector with 10 different encodings
#' @aliases encodedTexts
#' @examples
#' Encoding(data_char_encodedtexts)
#' data.frame(labelled = names(data_char_encodedtexts), 
#'            detected = encoding(data_char_encodedtexts)$all)
"data_char_encodedtexts"

#' internal data sets
#' 
#' Data sets used for mainly internal purposes by the \pkg{quanteda} package.
#' @name data-internal
#' @docType data
#' @keywords internal
NULL


#' @name inaugCorpus
#' @docType data
#' @title A corpus of US presidential inaugural addresses from 1789-2013
#' @description \code{inaugCorpus} is the \link{quanteda-package} corpus object of US presidents' inaugural addresses since 1789.
#' Document variables contain the year of the address and the last name of the president.
#' @examples
#' # some operations on the inaugural corpus
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
#' str(inaugTexts)
#' head(docvars(inaugCorpus), 10)
#' mycorpus <- corpus(inaugTexts)
NULL

