\name{dfm}
\alias{dfm}
\alias{dfm.corpus}
\title{Create a document-feature matrix from a corpus object}
\usage{
dfm(corpus, feature = c("word"), stem = FALSE, stopwords = FALSE,
  bigram = FALSE, groups = NULL, subset = NULL, verbose = TRUE,
  dictionary = NULL, dictionary.regex = FALSE, addto = NULL)

\method{dfm}{corpus}(corpus, feature = c("word"), stem = FALSE,
  stopwords = FALSE, bigram = FALSE, groups = NULL, subset = NULL,
  verbose = TRUE, dictionary = NULL, dictionary.regex = FALSE,
  addto = NULL)
}
\arguments{
  \item{corpus}{Corpus from which to generate the
  document-feature matrix}

  \item{feature}{Feature to count (e.g. words)}

  \item{stem}{Stem the words}

  \item{stopwords}{Remove stopwords}

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
  to the one named. If both dfms are built from
  dictionaries, the combined dfm will have its
  \code{Non_Dictionary} total adjusted.}
}
\value{
A data frame with row names equal to the document names and
column names equal to the feature labels.
}
\description{
returns a document by feature matrix compatible with
austin.  A typical usage would be to produce a
word-frequency matrix where the cells are counts of words
by document.
}
\examples{
data(iebudgets)
wfm <- dfm(iebudgets)

## by party, subset for 2010
wfmByParty2010 <- dfm(subset(iebudgets, year==2010), groups="party")

## with dictionaries
corpus <- subset(iebudgets, year==2010)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(corpus, dictionary=mydict)
dictDfm

## adding one dfm to another
mydict2 <- list(partyref=c("Lenihan", "Fianna", "Sinn", "Gael"))
dictDfm2 <- dfm(corpus, dictionary=mydict2, addto=dictDfm)
dictDfm2
}
\author{
Kenneth Benoit
}

