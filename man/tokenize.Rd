\name{tokenize}
\alias{tokenize}
\title{Split a string into words
The input text is split into words by whitespace}
\usage{
tokenize(str, langNorm = FALSE, removeDigits = TRUE, lower = TRUE,
  removePunct = TRUE)
}
\arguments{
  \item{str}{String to be tokenized}

  \item{langNorm}{If \code{TRUE} (default), French and
  German special characters are normalized}

  \item{removeDigits}{If \code{TRUE} (default), digits are
  removed}

  \item{lower}{If \code{TRUE} (default), string is
  converted to lowercase}

  \item{removePunct}{If \code{TRUE} (default), punctuation
  is removed}
}
\value{
a character vector containing the input text tokens
}
\description{
Split a string into words The input text is split into
words by whitespace
}
\examples{
testtxt <- "The quick brown fox named Séamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."
tokenize(testtxt)
tokenize(testtxt, lower=FALSE)
}

