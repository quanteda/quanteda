\name{bigrams}
\alias{bigrams}
\title{Create bigrams}
\usage{
bigrams(text, window = 1)
}
\arguments{
  \item{tokens}{}
}
\value{
a character of bigrams vector
}
\description{
Create bigrams
}
\examples{
data(iebudgets)
wfm <- dfm(iebudgets, group="party", bigram=TRUE)
bigrams("The quick brown fox jumped over the lazy dog.")
bigrams("The quick brown fox jumped over the lazy dog.", window=2)
}
\author{
kohei Watanabe
}

