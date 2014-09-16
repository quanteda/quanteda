\name{texts}
\alias{texts}
\alias{texts<-}
\title{get or set corpus texts}
\usage{
texts(corp)

texts(corp) <- value
}
\arguments{
  \item{corp}{A quanteda corpus object}

  \item{rownames}{If TRUE, overwrite the names of the
  documents with names from assigned object.}
}
\value{
For \code{texts}, a character vector of the texts in the
corpus.

For \code{texts <-}, the corpus with the updated texts.
}
\description{
Get or replace the texts in a quanteda corpus object.
}
\examples{
texts(inaugCorpus)[1]
sapply(texts(inaugCorpus), nchar)  # length in characters of the inaugual corpus texts

## this doesn't work yet - need to overload `[` for this replacement function
# texts(inaugTexts)[55] <- "GW Bush's second inaugural address, the condensed version."
}

