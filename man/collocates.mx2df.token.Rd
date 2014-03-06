\name{collocates.mx2df.token}
\alias{collocates.mx2df.token}
\title{This function extract collocates of a specific word from a collocation matrix}
\usage{
collocates.mx2df.token(token, index, mx, thresh = 0)
}
\arguments{
  \item{token}{Word of interest}

  \item{index}{Index created by collocates.createIndex()}

  \item{mx}{Collocation matrix created by
  collocates.findCollocation()}

  \item{thresh}{Threshhold of scores}
}
\value{
df2 List of collocates as data frame
}
\description{
This function extract collocates of a specific word from a
collocation matrix
}
\examples{
collocates <- collocates.mx2df.token('war', index, mx, 3)
}
\author{
Kohei Watanabent
}

