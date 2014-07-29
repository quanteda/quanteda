\name{tf}
\alias{tf}
\title{normalizes the term frequencies a dfm}
\usage{
tf(x)
}
\arguments{
  \item{dfm}{Document-feature matrix created by
  \code{\link{dfm}}}
}
\value{
A dfm matrix object where values are relative term
proportions within the document
}
\description{
Returns a matrix of term weights, as a \link{dfm} object
}
\examples{
data(iebudgets)
dtm <- dfm(iebudgets)
dtm[1:10, 100:110]
tf(dtm)[1:10, 100:110]
}
\author{
Ken Benoit
}

