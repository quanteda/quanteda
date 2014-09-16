\name{ndoc}
\alias{ndoc}
\alias{ndoc.corpus}
\alias{ndoc.dfm}
\title{get the number of documents}
\usage{
\emph{ }

\method{ndoc}{corpus}(corp)

\method{ndoc}{dfm}(x)
}
\arguments{
  \item{x}{a corpus or dfm object}
}
\value{
an integer (count) of the number of documents in the corpus
or dfm
}
\description{
Returns the number of documents in a corpus objects
}
\examples{
ndoc(inaugCorpus)
ndoc(dfm(inaugCorpus))
}

