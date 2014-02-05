\name{clean}
\alias{clean}
\title{Perform basic cleanup on a string
Simple cleanup for strings, removing punctuation, converting to lowercase
and optionally replacing some language-specific characters}
\usage{
clean(s, langNorm = FALSE, removeDigits = TRUE)
}
\arguments{
  \item{s}{String to be cleaned}
}
\value{
character vector in lowercase with punctuation (and
optionally digits) removed
}
\description{
Perform basic cleanup on a string Simple cleanup for
strings, removing punctuation, converting to lowercase and
optionally replacing some language-specific characters
}
\examples{
s <-"test"
clean(s)
}

