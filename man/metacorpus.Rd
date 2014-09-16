\name{metacorpus}
\alias{metacorpus}
\alias{metacorpus<-}
\title{get or set corpus metadata}
\usage{
metacorpus(corp, field = NULL)

metacorpus(corp, field) <- value
}
\arguments{
  \item{corp}{A quanteda corpus object}

  \item{field}{Metadata field name(s).  If \code{NULL}
  (default), return all metadata names.}
}
\value{
For \code{metacorpus}, a list of the metadata fields in the
corpus.  If a list is not what you wanted, you can wrap the
results in \link{unlist}, but this will remove any metadata
field that is set to \code{NULL}.

For \code{metacorpus <-}, the corpus with the updated
metadata.
}
\description{
Get or set the corpus-level metadata in a quanteda corpus
object.
}
\examples{
metacorpus(inaugCorpus)
metacorpus(inaugCorpus, "source")
metacorpus(inaugCorpus, "citation") <- "Presidential Speeches Online Project (2014)."
metacorpus(inaugCorpus, "citation")
}

