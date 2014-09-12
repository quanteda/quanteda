\name{dfmTrim}
\alias{dfmTrim}
\title{Trim a dfm based on a subset of features and words}
\usage{
dfmTrim(dfm, minCount = 5, minDoc = 5, sample = NULL, verbose = TRUE)
}
\arguments{
  \item{dfm}{Document-feature matrix created by
  \code{\link{dfm}}}

  \item{minCount}{minimum feature count}

  \item{minDoc}{minimum number of documents in which a
  feature appears}

  \item{sample}{how many features to retain (based on
  random selection)}

  \item{verbose}{print messages}
}
\value{
A dfm matrix object reduced in size.
}
\description{
Returns a document by feature matrix reduced in size based
on document and term frequency, and/or subsampling.
}
\examples{
data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dim(dtm)
dtmReduced <- dfmTrim(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
dim(dtmReduced)
dtmSampled <- dfmTrim(dtm, sample=50)  # top 200 words
dim(dtmSampled)  # 196 x 200 words
}
\author{
Will Lowe, adapted by Ken Benoit
}

