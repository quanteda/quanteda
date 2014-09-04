\name{dfm}
\alias{dfm}
\alias{dfm.character}
\alias{dfm.corpus}
\title{Create a document-feature matrix from a corpus object}
\usage{
dfm(corpus, feature = c("word"), stem = FALSE, stopwords = NULL,
  bigram = FALSE, groups = NULL, subset = NULL, verbose = TRUE,
  dictionary = NULL, dictionary.regex = FALSE, addto = NULL)

\method{dfm}{corpus}(corpus, feature = c("word"), stem = FALSE,
  stopwords = NULL, bigram = FALSE, groups = NULL, subset = NULL,
  verbose = TRUE, dictionary = NULL, dictionary.regex = FALSE,
  addto = NULL)

\method{dfm}{character}(textvec, feature = c("word"), stem = FALSE,
  stopwords = NULL, bigram = FALSE, verbose = TRUE, dictionary = NULL,
  dictionary.regex = FALSE, addto = NULL)
}
\arguments{
  \item{corpus}{Corpus from which to generate the
  document-feature matrix}

  \item{feature}{Feature to count (e.g. words)}

  \item{stem}{Stem the words}

  \item{stopwords}{A character vector of stopwords that
  will be removed from the text when constructing the
  \link{dfm}.  If \code{NULL} (default) then no stopwords
  will be applied.  If "TRUE" then it currently defaults to
  \code{\link{stopwords}}.}

  \item{groups}{Grouping variable for aggregating
  documents}

  \item{subset}{Expression for subsetting the corpus before
  processing}

  \item{verbose}{Get info to screen on the progress}

  \item{dictionary}{A list of character vector dictionary
  entries, including regular expressions (see examples)}

  \item{dictionary.regex}{\code{TRUE} means the dictionary
  is already in regular expression format, otherwise it
  will be converted from "wildcard" format}

  \item{addto}{\code{NULL} by default, but if an existing
  dfm object is specified, then the new dfm will be added
  to the one named. If both \link{dfm}'s are built from
  dictionaries, the combined dfm will have its
  \code{Non_Dictionary} total adjusted.}
}
\value{
A matrix object with row names equal to the document names
and column names equal to the feature labels. This matrix
has \code{names(dimnames) = c("docs", "words")} to make it
conformable to an \link[austin]{wfm} object.
}
\description{
returns a document by feature matrix compatible with
austin.  A typical usage would be to produce a
word-frequency matrix where the cells are counts of words
by document.
}
\examples{
data(inaugCorpus)
wfm <- dfm(inaugCorpus)

## by president, after 1960
wfmByPres2010 <- dfm(subset(inaugCorpus, year>1960), groups="president")

## with dictionaries
corpus <- subset(inaugCorpus, year>1960)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(corpus, dictionary=mydict)
dictDfm

## removing stopwords
testText <- "The quick brown fox named Séamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."#
testCorpus <- corpus(testText)
dfm(testCorpus, stopwords=TRUE)
if (require(tm)) {
}

## adding one dfm to another
mydict2 <- list(partyref=c("Lenihan", "Fianna", "Sinn", "Gael"))
dictDfm2 <- dfm(corpus, dictionary=mydict2, addto=dictDfm)
dictDfm2
}
\author{
Kenneth Benoit
}

