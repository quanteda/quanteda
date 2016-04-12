
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

#' @name ukimmigTexts
#' @title Immigration-related sections of 2010 UK party manifestos
#' @docType data
#' @description Extracts from the election manifestos of 9 UK political parties from 2010, related
#' to immigration or asylum-seekers.
#' @format A named character vector of plain ASCII texts
#' @examples
#' ukimmigCorpus <- corpus(ukimmigTexts, docvars=data.frame(party=names(ukimmigTexts)))
#' metadoc(ukimmigCorpus, "language") <- "english"
#' summary(ukimmigCorpus, showmeta = TRUE)
NULL

#' @name syllables
#' @aliases englishSyllables
#' @rdname syllables
#' @docType data
#' @source \code{englishSyllables} is built from the freely available CMU pronunciation dictionary at \code{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}.
#' 
NULL

#' @name LBGexample
#' @docType data
#' @title dfm with example data from Table 1 of Laver Benoit and Garry
#'   (2003)
#' @description Constructed example data to demonstrate the Wordscores algorithm, from Laver
#'   Benoit and Garry (2003), Table 1.
#' @details This is the example word count data from Laver, Benoit and Garry's
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions:
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @format A \link{dfm} object with 6 documents and 37 features
#' @references Laver, Michael, Kenneth Benoit, and John Garry.  2003.
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Estimating policy
#'   positions from political text using words as data.}" \emph{American Political
#'   Science Review} 97(2): 311-331.
NULL

#' @name ie2010Corpus
#' @aliases iebudgets
#' @title Irish budget speeches from 2010
#' @description Speeches and document-level variables from the debate over the 
#'   Irish budget of 2010.
#' @format The corpus object for the 2010 budget speeches, with document-level 
#'   variables for year, debate, serial number, first and last name of the 
#'   speaker, and the speaker's party.
#' @source Lowe, Will, and Kenneth R Benoit. 2013. "Validating Estimates of
#'   Latent Traits From Textual Data Using Human Judgment as a Benchmark."
#'   \emph{Political Analysis} 21: 298-313.
#' @docType data
#' @examples
#' summary(ie2010Corpus)
NULL

# data(ie2010Corpus, package="quantedaData")
# txts <- texts(ie2010Corpus)
# for (i in 1:length(txts)) {
#     temptxt <- system2("native2ascii", input = txts[i], stdout = TRUE)
#     temptxt[temptxt==""] <- "\n"
#     temptxt <- paste(temptxt, collapse="")
#     txts[i] <- temptxt
# }
# texts(ie2010Corpus) <- txts
# save(ie2010Corpus, file="data/ie2010Corpus.RData")

#' @name exampleString
#' @title A paragraph of text for testing various text-based functions
#' @description This is a long paragraph (2,914 characters) of text taken from an Irish budget speech by Joe Higgins.
#' @format character vector with one element
#' @docType data
#' @examples
#' tokenize(exampleString, removePunct = TRUE)
NULL

#' @name encodedTexts
#' @title encoded texts for testing
#' @description \code{encodedTexts} is a 10-element character vector with 10 different encodings
#' @docType data
#' @examples
#' Encoding(encodedTexts)
#' data.frame(labelled = names(encodedTexts), detected = encoding(encodedTexts)$all)
NULL


#' @name wordlists
#' @title word lists used in some readability indexes
#' @description \code{wordlists} is a named list of character vectors where each list element corresponds to a different readability index.  These are:
#' \describe{
#' \item{\code{DaleChall}}{The long Dale-Chall list of 3,000 familiar (English) words needed to compute the Dale-Chall Readability Formula.}
#' \item{\code{Spache}}{The revised Spache word list (see Klare 1975, 73) needed to compute the Spache Revised Formula of readability (Spache 1974.}
#' }
#' @references 
#' Chall, J. S., & Dale, E.  1995. \emph{Readability Revisited: The New Dale-Chall Readability Formula}. Brookline Books.
#' 
#' Klare, G. R. 1975. "Assessing readability." \emph{Reading Research Quarterly} 10(1): 62–102.
#' 
#' Spache, G. 1953. "A new readability formula for primary-grade reading materials." \emph{The Elementary School Journal} 53: 410-413.
#' @docType data
NULL
# makeWordList <- function(filename) {
#     wordList <- textfile(filename, cache = FALSE)@texts
#     wordList <- stringi::stri_replace_all_regex(wordList, "-", "_")
#     wordList <- tokenize(wordList, simplify = TRUE)
#     wordList <- stringi::stri_replace_all_regex(wordList, "_", "-")
#     wordList
# }
# dalechall    <- makeWordList("~/Dropbox/QUANTESS/quanteda_working_files/readability/Dale-Chall.txt")
# spache    <- makeWordList("~/Dropbox/QUANTESS/quanteda_working_files/readability/Spache.txt")
# wordlists <- list(dalechall = dalechall, spache = spache)
# save(wordlists, file = "data/wordlists.RData")


#' @name encodedTextFiles
#' @title a .zip file of texts containing a variety of differently encoded texts
#' @source The Universal Declaration of Human Rights resources, 
#'   \url{http://www.ohchr.org/EN/UDHR/Pages/SearchByLang.aspx}
#' @docType data
#' @description A set of translations of the Universal Declaration of Human
#'   Rights, plus one or two other miscellaneous texts, for testing the text
#'   input functions that need to translate different input encodings.
#' @examples
#' \dontrun{# unzip the files to a temporary directory
#' FILEDIR <- tempdir()
#' unzip(system.file("extdata", "encodedTextFiles.zip", package = "quanteda"), exdir = FILEDIR)
#' 
#' # get encoding from filename
#' filenames <- list.files(FILEDIR, "\\.txt$")
#' # strip the extension
#' filenames <- gsub(".txt$", "", filenames)
#' parts <- strsplit(filenames, "_")
#' fileencodings <- sapply(parts, "[", 3)
#' fileencodings
#' 
#' # find out which conversions are unavailable (through iconv())
#' cat("Encoding conversions not available for this platform:")
#' notAvailableIndex <- which(!(fileencodings %in% iconvlist()))
#' fileencodings[notAvailableIndex]
#' 
#' # try textfile
#' require(quanteda)
#' tfile <- textfile(paste0(FILEDIR, "/", "*.txt"))
#' substring(texts(tfile)[1], 1, 80) # gibberish
#' substring(texts(tfile)[4], 1, 80) # hex
#' substring(texts(tfile)[40], 1, 80) # hex
#' 
#' # read them in again
#' tfile <- textfile(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings)
#' substring(texts(tfile)[1], 1, 80)  # English
#' substring(texts(tfile)[4], 1, 80)  # Arabic, looking good 
#' substring(texts(tfile)[40], 1, 80) # Cyrillic, looking good
#' substring(texts(tfile)[7], 1, 80)  # Chinese, looking good
#' substring(texts(tfile)[26], 1, 80) # Hindi, looking good
#' 
#' tfile <- textfile(paste0(FILEDIR, "/", "*.txt"), encoding = fileencodings,
#'                   docvarsfrom = "filenames", 
#'                   docvarnames = c("document", "language", "inputEncoding"))
#' encodingCorpus <- corpus(tfile, source = "Created by encoding-tests.R") 
#' summary(encodingCorpus)
#' }
NULL


#' @name mobydickText
#' @title Project Gutenberg text of Herman Melville's \emph{Moby Dick}
#' @source Project Gutenberg, \url{http://www.gutenberg.org}
#' @docType data
#' @description Named character object of the ASCII text of Herman Melville's
#'   \emph{Moby Dick}, EBook no. 2701.
#' @examples
#' summary(mobydickText)
NULL
