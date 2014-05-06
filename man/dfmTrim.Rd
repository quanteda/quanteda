\name{dfmTrim}
\alias{dfmTrim}
\title{Trim a dfm based on a subset of features and words}
\usage{
dfmTrim(dfm, min.count = 5, min.doc = 5, sample = NULL, verbose = TRUE)
}
\arguments{
  \item{dfm}{Document-feature matrix created by
  \code{\link{dfm}}}

  \item{min.count}{minimum feature count}

  \item{min.doc}{minimum number of documents in which a
  feature appears}

  \item{sample}{how many features to retain (based on
  random selection)}
}
\value{
A dfm matrix object reduced in size.
}
\description{
Returns a document by feature matrix reduced in size based
on document and term frequency, and/or subsampling.
}
\examples{
data(iebudgets)
dtm <- dfm(iebudgets)
dim(dtm)  # 196 docs x 13343 words
dtm.reduced <- dfmTrim(dtm, min.count=10, min.doc=3) # only words occuring at least 10 times and in at least 3 documents
dim(dtm.reduced)  # 196 docs x 3006 words
dtm.sampled <- dfmTrim(dtm, sample=200)  # top 200 words
dim(dtm.sampled)  # 196 x 200 words
}
\author{
Will Lowe, adapted by Ken Benoit
}

