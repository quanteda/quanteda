\name{collocations}
\alias{collocations}
\title{Detect collocations in a text}
\usage{
collocations(text = NULL, file = NULL, top = NA, distance = 2, n = 2,
  method = c("lr", "chi2", "mi"))
}
\arguments{
  \item{text}{a text or vector of texts}

  \item{file}{a filename containing a text}

  \item{top}{threshold number for number of collocations to
  be returned (in descending order of association value)}

  \item{distance}{distance between pairs of collocations}

  \item{method}{association measure for detecting
  collocations}

  \item{n}{Only bigrams (n=2) implemented so far.}
}
\value{
A list of collocations, their frequencies, and their test
statistics
}
\description{
returns a list of collocations.  Note: Currently works only
for pairs (bigram collocations).
}
\examples{
data(inaugCorpus)
collocations(texts(inaugCorpus)[1], top=50)
collocations(texts(inaugCorpus)[1], top=50, method="chi2")
}
\author{
Kenneth Benoit
}

