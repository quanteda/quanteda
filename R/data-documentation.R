#' a paragraph of text for testing various text-based functions
#' 
#' This is a long paragraph (2,914 characters) of text taken from an Irish budget speech by Joe Higgins.
#' @format character vector with one element
#' @keywords data
#' @examples
#' tokenize(data_char_sampletext, removePunct = TRUE)
"data_char_sampletext"

#' text of Herman Melville's Moby Dick
#' 
#' Named character object of the ASCII text of Herman Melville's
#'  \emph{Moby Dick}, EBook no. 2701.
#' @source Project Gutenberg, \url{http://www.gutenberg.org}
#' @keywords data
#' @examples
#' summary(data_char_mobydick)
"data_char_mobydick"

#' immigration-related sections of 2010 UK party manifestos
#' 
#' Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @keywords data
#' @examples
#' data_corpus_ukimmig2010 <- 
#'     corpus(data_char_ukimmig2010, 
#'            docvars = data.frame(party = names(ukimmigTexts)))
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
#' @format A \link{dfm} object with 6 documents and 37 features
#' @references Laver, Michael, Kenneth Benoit, and John Garry.  2003. 
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating policy 
#'   positions from political text using words as data.}" \emph{American
#'   Political Science Review} 97(2): 311-331.
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
#' @keywords data
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


#' internal data sets
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
#' @format a \link{corpus} object with docvars including year, and the last and
#'   first names of the presidents delivering the inaugural address
#' @examples
#' # some operations on the inaugural corpus
#' summary(data_corpus_inaugural)
#' head(docvars(data_corpus_inaugural), 10)
#' @references 
#' \url{https://archive.org/details/Inaugural-Address-Corpus-1789-2009} and 
#' \url{http://www.presidency.ucsb.edu/inaugurals.php}.
#' @keywords data
"data_corpus_inaugural"


#' @rdname data_corpus_inaugural
#' @details \code{data_char_inaugural} is the character vector of US presidential inaugaration speeches
#' @examples
#' # working with the character vector only
#' str(data_char_inaugural)
#' head(docvars(data_corpus_inaugural), 10)
#' mycorpus <- corpus(data_char_inaugural)
#' @keywords data
"data_char_inaugural"

