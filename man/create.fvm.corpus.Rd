\name{create.fvm.corpus}
\alias{create.fvm.corpus}
\title{Create a feature-value matrix from a corpus object
returns a feature value matrix compatible with austin}
\usage{
create.fvm.corpus(corpus, feature = c("word"), stem = FALSE,
  remove_stopwords = FALSE, groups = NULL, subset = NULL,
  verbose = TRUE)
}
\arguments{
  \item{corpus}{Corpus to make matrix from}

  \item{feature}{Feature to count}

  \item{feature}{type to aggregate by, default is file}
}
\description{
Create a feature-value matrix from a corpus object returns
a feature value matrix compatible with austin
}
\examples{
fvm <- create.fvm.corpus(budgets, group="party")
}

