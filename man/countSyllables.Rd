\name{countSyllables}
\alias{countSyllables}
\title{Returns a count of the number of syllables in the input
This function takes a text and returns a count of the number of syllables it contains.
For British English words, the syllable count is exact and looked up from the CMU
pronunciation dictionary. For any word not in the dictionary the syllable count
is estimated by counting vowel clusters.}
\usage{
countSyllables(sourceText, verbose = FALSE)
}
\arguments{
  \item{sourceText}{Text to be counted}

  \item{verbose}{If True, print out the count. Default
  false.}
}
\value{
numeric A count (estimate) of the number of syllables in
sourceText
}
\description{
Returns a count of the number of syllables in the input
This function takes a text and returns a count of the
number of syllables it contains. For British English words,
the syllable count is exact and looked up from the CMU
pronunciation dictionary. For any word not in the
dictionary the syllable count is estimated by counting
vowel clusters.
}
\examples{
countSyllables("This is an example sentence.")
}

