\name{dfmSort}
\alias{dfmSort}
\title{sort a dfm by one or more margins}
\usage{
dfmSort(x, margin = c("words", "docs", "both"), decreasing = TRUE)
}
\arguments{
  \item{dfm}{Document-feature matrix created by
  \code{\link{dfm}}}

  \item{margin}{which margin to sort on \code{words} to
  sort words, \code{docs} to sort documents, and
  \code{both} to sort both}

  \item{decreasing}{TRUE (default) if sort will be in
  descending order}
}
\value{
A sorted \link{dfm} matrix object
}
\description{
Sorts a \link{dfm} by documents or words
}
\examples{
data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[, 1:10]
dtm <- dfmSort(dtm, "words")
dfmSort(dtm)[, 1:10]
dfmSort(dtm, "both")[, 1:10]
}
\author{
Ken Benoit
}

