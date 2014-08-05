\name{dfmSample}
\alias{dfmSample}
\title{Corpus sampling}
\usage{
dfmSample(docMat, size = n, replace = FALSE, prob = NULL)
}
\arguments{
  \item{corpus}{An existing corpus to be sampled}

  \item{size}{A positive number, the number of texts to
  return}

  \item{replace}{Should sampling be with replacement?}

  \item{prob}{Not implemented}
}
\description{
Takes a random sample of the specified size from a corpus,
with or without replacement
}
\examples{
data(movies)
d <- dfm(movies)
samp <- dfmSample(d, 100, replace=TRUE)
}

