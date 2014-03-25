\name{clean}
\alias{clean}
\title{Perform basic cleanup on a character object}
\usage{
clean(s, langNorm = FALSE, removeDigits = TRUE, lower = TRUE)
}
\arguments{
  \item{s}{character object to be cleaned}
}
\value{
character object in lowercase with punctuation (and
optionally digits) removed
}
\description{
Simple cleanup for strings, removing punctuation,
converting to lowercase and optionally replacing some
language-specific characters
}
\examples{
s <- "A cursed £$&^!€ Exclamation! point; paragraph §1.2, which I wrote."
clean(s)
}

