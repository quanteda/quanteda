\name{clean}
\alias{clean}
\alias{clean.character}
\alias{clean.corpus}
\title{clean a set of texts}
\usage{
clean(x, ...)

\method{clean}{character}(x, removeDigits = TRUE, removePunct = TRUE,
  lower = TRUE, ...)

\method{clean}{corpus}(x, removeDigits = TRUE, removePunct = TRUE,
  lower = TRUE, ...)
}
\arguments{
  \item{x}{The text to be cleaned}

  \item{x}{The character vector to be cleaned}

  \item{removeDigits}{Should digits be removed? TRUE or
  FALSE}

  \item{removePunct}{Should punctuation be removed? TRUE or
  FALSE}

  \item{lower}{Convert to lower case? TRUE or FALSE}

  \item{x}{The character vector to be cleaned}

  \item{removeDigits}{Should digits be removed? TRUE or
  FALSE}

  \item{removePunct}{Should punctuation be removed? TRUE or
  FALSE}

  \item{lower}{Convert to lower case? TRUE or FALSE}
}
\value{
a corpus with the texts cleaned.
}
\description{
clean the texts from a character vector or from a corpus.

clean the texts from a character vector by removing
(optionally) digits and punctuation, and converting to
lower case

clean the texts in a corpus by removing (optionally) digits
and punctuation, and converting to lower case. Returns a
corpus with the texts cleaned.
}
\examples{
clean(inaugTexts, removeDigits=FALSE)
clean(inaugCorpus, removeDigits=FALSE)
}

