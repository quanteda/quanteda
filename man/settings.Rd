\name{settings}
\alias{settings}
\alias{settings<-}
\title{Get or set the corpus settings}
\usage{
settings(corp, fields = NULL)

settings(corp, fields) <- value
}
\arguments{
  \item{corp}{Corpus from/to which settings are queried or
  applied}

  \item{fields}{a valid corpus setting field name}
}
\description{
Get or set various settings in the corpus for the treatment
of texts, such as rules for stemming, stopwords,
collocations, etc. \code{settings(corp)} query the corps
settings \code{settings(corp, settingname) <-} update the
corpus settings
}

