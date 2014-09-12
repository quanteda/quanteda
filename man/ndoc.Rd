\name{ndoc}
\alias{ndoc}
\alias{ndoc.corpus}
\title{get the number of documents}
\usage{
ndoc(x)

\method{ndoc}{corpus}(corp)
}
\arguments{
  \item{corp}{a quanteda corpus object}
}
\value{
an integer (count) of the number of documents in the corpus
}
\description{
Returns the number of documents.

Returns the number of documents in a corpus objects
}
\examples{
ndoc(inaugCorpus)
}

