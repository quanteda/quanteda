\name{dfm2tmformat}
\alias{dfm2tmformat}
\title{Convert a quanteda \code{\link{dfm}} (document feature matrix)
 into a
 \pkg{tm} \link[tm:DocumentTermMatrix]{DocumentTermMatrix}}
\usage{
dfm2tmformat(d, weighting = weightTf, ...)
}
\arguments{
  \item{d}{A \link[quanteda]{dfm} object}

  \item{weight}{\pkg{tm}'s coercion function accepts
  weightings such as tf-idf, see \pkg{tm}'s
  \link[tm]{as.DocumentTermMatrix} for a list of possible
  arguments. The default is just tf (term frequency)}
}
\value{
A triplet matrix of class \link[tm]{as.DocumentTermMatrix}
}
\description{
\pkg{tm} represents sparse document-feature matrixes in the
\link[slam:matrix]{simple triplet matrix} format of the
package \pkg{slam}. This function converts a \code{dfm}
into a \link[tm:DocumentTermMatrix]{DocumentTermMatrix},
for working with the \code{dfm} in \pkg{tm} or in other
packages that expect this format, such as
\pkg{topicmodels}.
}
\examples{
data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
d <- dfmTrim(dfm(iebudgets2010), minCount=5, minDoc=3)
dim(d)
td <- dfm2tmformat(d)
length(td$v)
if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)
}

