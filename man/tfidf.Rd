\name{tfidf}
\alias{tfidf}
\title{compute the tf-idf weights of a dfm}
\usage{
tfidf(x, normalize = TRUE)
}
\arguments{
  \item{dfm}{Document-feature matrix created by
  \code{\link{dfm}}}

  \item{normalize}{whether to normalize term frequency by
  document totals}
}
\value{
A dfm matrix object where values are tf-idf weights
}
\description{
Returns a matrix of tf-idf weights, as a \link{dfm} object
}
\examples{
data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[1:10, 100:110]
tfidf(dtm)[1:10, 100:110]
tfidf(dtm, normalize=FALSE)[1:10, 100:110]
}
\author{
Ken Benoit
}

