\name{corpus.reshape}
\alias{corpus.reshape}
\title{Transform a corpus by splitting texts into sentences
Each text in the corpus is split into sentences, and each
sentence becomes a standalone text, with attributes indicating
the text it is taken from and it's serial number in that text}
\usage{
corpus.reshape(corpus)
}
\arguments{
  \item{corpus}{Corpus to transform}

  \item{feature}{Feature to count}
}
\description{
Transform a corpus by splitting texts into sentences Each
text in the corpus is split into sentences, and each
sentence becomes a standalone text, with attributes
indicating the text it is taken from and it's serial number
in that text
}
\examples{
sentCorp <- corpus.reshape(corpus)
}

