\name{countSyllables}
\alias{countSyllables}
\title{Returns a count of the number of syllables in the input
This function takes a text and returns a count of the number of syllables it contains.
For British English words, the syllable count is exact and looked up from the CMU
pronunciation dictionary. For any word not in the dictionary the syllable count
is estimated by counting vowel clusters.}
\usage{
  countSyllables(sourceText)
}
\arguments{
  \item{text}{Text to be counted}
}
\description{
  Returns a count of the number of syllables in the input
  This function takes a text and returns a count of the
  number of syllables it contains. For British English
  words, the syllable count is exact and looked up from the
  CMU pronunciation dictionary. For any word not in the
  dictionary the syllable count is estimated by counting
  vowel clusters.
}
\examples{
tokenize("This is an example sentence.")
}

