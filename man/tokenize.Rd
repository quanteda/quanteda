\name{tokenize}
\alias{tokenize}
\alias{tokenize.character}
\alias{tokenize.corpus}
\title{tokenize a set of texts}
\usage{
tokenize(x, ...)

\method{tokenize}{character}(text, clean = TRUE, simplify = FALSE)

\method{tokenize}{corpus}(corpus, ...)
}
\value{
A list of length \code{\link{ndoc}(texts)} of the tokens
found in each text.
}
\description{
Tokenize the texts from a character vector or from a
corpus.
}

