\name{collocates.mx2dist}
\alias{collocates.mx2dist}
\title{This function converts a collocation matrix to a distance matrix that can be used in hclust() in 'graphics' pachage}
\usage{
collocates.mx2dist(mx)
}
\arguments{
  \item{mx}{collocation matrix created by
  collocates.findCollocation()}
}
\value{
distance matrix for hclust()
}
\description{
This function converts a collocation matrix to a distance
matrix that can be used in hclust() in 'graphics' pachage
}
\examples{
dist <- collocates.mx2dist(mx)
hc <- hclust(dist, "ave")
plot(hc, hang = -1)
}
\author{
Kohei Watanabe
}

