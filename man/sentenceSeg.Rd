\name{sentenceSeg}
\alias{sentenceSeg}
\title{split a text into sentences
This function takes a text and splits it into sentences.}
\usage{
sentenceSeg(text, pat = "[\\\\.\\\\?\\\\!][\\\\n* ]|\\\\n\\\\n*",
  abbreviations = NULL, parag = TRUE)
}
\arguments{
  \item{text}{Text to be segmented}
}
\description{
split a text into sentences This function takes a text and
splits it into sentences.
}
\examples{
test <- "This is a sentence! Several sentences. It's designed by a Dr. to test whether this function works. Or not? Or not."
sentenceSeg(test)
}

