\name{ngrams}
\alias{ngrams}
\title{Create ngrams}
\usage{
ngrams(text, n = 2, window = 1)
}
\arguments{
  \item{tokens}{}
}
\value{
a character of bigrams vector
}
\description{
Create ngrams
}
\examples{
data(iebudgets)
ngrams("The quick brown fox jumped over the lazy dog.", n=2)
ngrams("The quick brown fox jumped over the lazy dog.", n=3)
}
\author{
Kohei Watanabe, Ken Benoit, Paul Nulty
}

