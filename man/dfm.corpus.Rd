\name{dfm.corpus}
\alias{dfm.corpus}
\title{Create a document-feature matrix from a corpus object}
\usage{
dfm.corpus(corpus, feature = c("word"), stem = FALSE, stopwords = FALSE,
  groups = NULL, subset = NULL, verbose = TRUE)
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
}
\description{
returns a document by feature matrix compatible with
austin.  A typical usage would be to produce a
word-frequency matrix where the cells are counts of words
by document.
}
\examples{
#' \dontrun{
data(iebudgets)
wfm <- dfm.corpus(iebudgets)
wfmByParty2010 <- dfm.corpus(iebudgets, groups="party", subset=(year==2010))
}
}

