\name{trimdfm}
\alias{trimdfm}
\title{Trim a dfm based on a subset of features and words}
\usage{
trimdfm(x, minCount = 5, minDoc = 5, sample = NULL, verbose = TRUE)
}
\arguments{
  \item{x}{document-feature matrix created by \link{dfm}}

  \item{minCount}{minimum feature count}

  \item{minDoc}{minimum number of documents in which a
  feature appears}

  \item{sample}{how many features to retain (based on
  random selection)}

  \item{verbose}{print messages}
}
\value{
A \link{dfm} object reduced in size.
}
\description{
Returns a document by feature matrix reduced in size based
on document and term frequency, and/or subsampling.
}
\examples{
data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dim(dtm)
dtmReduced <- trimdfm(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
dim(dtmReduced)
dtmSampled <- trimdfm(dtm, sample=200)  # top 200 words
dim(dtmSampled)  # 196 x 200 words
}
\author{
Ken Benoit adapted from code by Will Lowe (see
\link[austin]{trim})
}

