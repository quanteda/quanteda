\name{ngrams}
\alias{ngrams}
\title{Create ngrams}
\usage{
ngrams(text, n = 2, window = 1, concatenator = "_")
}
\arguments{
  \item{text}{character vector containing the texts from
  which ngrams will be extracted}

  \item{n}{the number of tokens to concatenate. Default is
  2 for bigrams.}

  \item{window}{how many words to be counted for adjacency.
  Default is 1 for only immediately neighbouring words.}

  \item{concatenator}{character for combining words,
  default is \code{_} (underscore) character}
}
\value{
a character of bigrams vector
}
\description{
Create a set of ngrams (words in sequence) from a text.
}
\examples{
data(iebudgets)
ngrams("The quick brown fox jumped over the lazy dog.", n=2)
ngrams("The quick brown fox jumped over the lazy dog.", n=3)
}
\author{
Kohei Watanabe, Ken Benoit, Paul Nulty
}

