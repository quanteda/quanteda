\name{clean}
\alias{clean}
\title{Perform basic cleanup on a character object
Simple cleanup for strings, removing punctuation, converting to lowercase
and optionally replacing some language-specific characters}
\usage{
clean(s, langNorm = FALSE, removeDigits = TRUE)
}
\arguments{
  \item{s}{character object to be cleaned}
}
\value{
character object in lowercase with punctuation (and
optionally digits) removed
}
\description{
Perform basic cleanup on a character object Simple cleanup
for strings, removing punctuation, converting to lowercase
and optionally replacing some language-specific characters
}
\examples{
s <- "A cursed £$&^!
}

