\name{dfm}
\alias{dfm}
\alias{dfm.corpus}
\title{Create a document-feature matrix from a corpus object}
\usage{
dfm(corpus, feature = c("word"), stem = FALSE, stopwords = FALSE,
  groups = NULL, subset = NULL, verbose = TRUE, dictionary = NULL,
  dictionary.regex = FALSE)

\method{dfm}{corpus}(corpus, feature = c("word"), stem = FALSE,
  stopwords = FALSE, groups = NULL, subset = NULL, verbose = TRUE,
  dictionary = NULL, dictionary.regex = FALSE)
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
\dontrun{
wfmByParty2010 <- dfm(iebudgets, groups="party", subset=(year==2010))
}
## with dictionaries
corpus <- subset(iebudgets, year==2010)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(corpus, dictionary=mydict)
}
\author{
Kenneth Benoit
}

