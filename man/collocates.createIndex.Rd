\name{collocates.createIndex}
\alias{collocates.createIndex}
\title{This function creates a index for faster access to matricis}
\usage{
collocates.createIndex(keys)
}
\arguments{
  \item{keys}{All words to be indexed as vector fromat}
}
\value{
index List that allows fast referece to column/row number
}
\description{
This function creates a index for faster access to matricis
}
\examples{
index <- collocates.createIndex(row.names(dfm))
}
\author{
Kohei Watanabe
}

