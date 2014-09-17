\name{tokenize}
\alias{tokenize}
\alias{tokenize.character}
\alias{tokenize.corpus}
\title{tokenize a set of texts}
\usage{
tokenize(x, ...)

\method{tokenize}{character}(x, clean = FALSE, simplify = FALSE, ...)

\method{tokenize}{corpus}(corpus, ...)
}
\arguments{
  \item{x}{The text to be tokenized}

  \item{x}{A character vector to be tokenized}
}
\value{
A list of length \code{\link{ndoc}(texts)} of the tokens
found in each text.

A list of length \code{\link{ndoc}(texts)} of the tokens
found in each text.
}
\description{
Tokenize the texts from a character vector or from a
corpus.
}

