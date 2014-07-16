\name{corpusReshape}
\alias{corpusReshape}
\title{Transform a corpus by splitting texts into sentences}
\usage{
corpusReshape(corpus)
}
\arguments{
  \item{corpus}{Corpus to transform}
}
\description{
Each text in the corpus is split into sentences, and each
sentence becomes a standalone text, with attributes
indicating the text it is taken from and it's serial number
in that text
}
\examples{
\dontrun{
corpus <- data(iebudgets)
sentCorp <- corpus.reshape(corpus)
}
}

