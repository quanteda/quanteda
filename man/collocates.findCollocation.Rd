\name{collocates.findCollocation}
\alias{collocates.findCollocation}
\title{This function finds collocates in a given texts}
\usage{
collocates.findCollocation(method, index, texts, window = 5, smooth = 1,
  segment = TRUE)
}
\arguments{
  \item{method}{Method to calculate collocation scores}

  \item{index}{Index created by collocates.createIndex()}

  \item{text}{Text files}

  \item{window}{Window span of collocation}

  \item{smooth}{Constant to be added to word frequency}

  \item{segment}{Separation by sentences}
}
\value{
smx Collocaton matrix
}
\description{
This function finds collocates in a given texts
}
\examples{
mx <- collocates.findCollocation('pmi', index, texts, 2.2e-16, TRUE)
}
\author{
Kohei Watanabent
}

