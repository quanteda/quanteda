\name{subset.corpus}
\alias{subset.corpus}
\title{extract a subset of a corpus}
\usage{
\method{subset}{corpus}(corpus, subset = NULL, select = NULL)
}
\arguments{
  \item{corpus}{corpus object to be subsetted.}

  \item{subset}{logical expression indicating elements or
  rows to keep: missing values are taken as false.}

  \item{select}{expression, indicating the attributes to
  select from the corpus}
}
\description{
Works just like the normal subset command but for corpus
objects
}
\examples{
\dontrun{
data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
summary(iebudgets2010)
iebudgetsLenihan <- subset(iebudgets, speaker="Lenihan", select=c(speaker, year))
summary(iebudgetsLenihan)
}
}

