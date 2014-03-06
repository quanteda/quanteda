\name{collocates.mx2df.score}
\alias{collocates.mx2df.score}
\title{This function converts a collocation matrix to a list of collocates as data frame}
\usage{
collocates.mx2df.score(thresh = 0, index, mx)
}
\arguments{
  \item{thresh}{Threshhold of scores}

  \item{index}{Index created by collocates.createIndex()}

  \item{mx}{Collocation matrix created by
  collocates.findCollocation()}
}
\value{
df2 List of collocates as data frame
}
\description{
This function converts a collocation matrix to a list of
collocates as data frame
}
\examples{
collocates <- collocates.mx2df.score(3, index, mx)
}
\author{
Kohei Watanabe
}

