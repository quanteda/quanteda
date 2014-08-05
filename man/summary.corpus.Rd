\name{summary.corpus}
\alias{summary.corpus}
\title{Corpus summary}
\usage{
\method{summary}{corpus}(corpus, nmax = 100, texts = "texts",
  subset = NULL, verbose = TRUE)
}
\arguments{
  \item{corpus}{An existing corpus to be summarized}

  \item{nmax}{maximum number of texts to describe,
  default=100}

  \item{texts}{The name of the attribute containing the
  corpus texts, if not 'texts'.  For instance, if the
  corpus contained translated texts as an attribute, then
  setting this to the name of that variable would make it
  possible to summarize the alternate rather than the main
  texts.}

  \item{subset}{a Boolean expression that specifies a
  subset of the texts, similar to \code{subset.corpus}}

  \item{verbose}{FALSE to turn off printed output}
}
\description{
Displays information about a corpus object, including
attributes and metadata such as date of number of texts,
creation and source.
}
\examples{
data(iebudgets)
subset(iebudgets, year==2010)
summary(iebudgets, nmax=10)
}

