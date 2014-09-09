\name{summary.corpus}
\alias{summary.corpus}
\title{Corpus summary}
\usage{
\method{summary}{corpus}(corp, n = 100, verbose = TRUE,
  printdocnames = TRUE)
}
\arguments{
  \item{corp}{corpus to be summarized}

  \item{n}{maximum number of texts to describe,
  default=100}

  \item{verbose}{FALSE to turn off printed output}
}
\description{
Displays information about a corpus object, including
attributes and metadata such as date of number of texts,
creation and source.
}
\examples{
summary(inaugCorpus)
summary(inaugCorpus, n=10, printdocnames=FALSE)
}

