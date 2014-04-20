\name{bigrams}
\alias{bigrams}
\title{Create bigrams}
\usage{
bigrams(text, window = 1)
}
\arguments{
  \item{text}{character vector containing the texts from
  which ngrams will be extracted}

  \item{window}{how many words to be counted for adjacency.
  Default is 1 for only immediately neighbouring words.}
}
\value{
a character of bigrams vector
}
\description{
Create bigrams
}
\examples{
data(iebudgets)
bigrams("The quick brown fox jumped over the lazy dog.")
bigrams("The quick brown fox jumped over the lazy dog.", window=2)
}
\author{
Kohei Watanabe and Ken Benoit
}

