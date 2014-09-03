\name{sample.corpus}
\alias{sample.corpus}
\title{Corpus sampling}
\usage{
sample.corpus(corpus, size = n, replace = FALSE, prob = NULL)
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
movieSamp <- sample(movies, 200, replace=TRUE)
}

