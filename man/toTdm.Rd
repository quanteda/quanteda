\name{toTdm}
\alias{toTdm}
\title{Convert a quanteda dfm (document feature matrix) into a tm term-document triplet matrix}
\usage{
toTdm(d, weight = weightTf)
}
\arguments{
  \item{d}{A matrix object with row names equal to the
  document names and column names equal to the feature
  labels, as returned by dfm()}

  \item{weight}{Tm's coercion function accepts weightings
  such as tf idf, see tm's as.termDocumentMatrix for list
  of possible args. The default is just tf (term
  frequency)}
}
\value{
A triplet matrix of class TermDocumentMatrix
}
\description{
tm uses a triplet matrix representation to represent term
frequency per document. This function uses slam's coercion
function to convert a standard feature matrix to this
format for compatibility with #' packages that use this
representation
}
\examples{
data(movies)
d <- dfm(movies)
td <- toTdm(d)
}

