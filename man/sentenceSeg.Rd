\name{sentenceSeg}
\alias{sentenceSeg}
\title{split a text into sentences
This function takes a text and splits it into sentences.}
\usage{
sentenceSeg(txt, pat = "[\\\\.\\\\?\\\\!][\\\\n* ]|\\\\n\\\\n*",
  abbreviations = NULL, stripempty = TRUE)
}
\arguments{
  \item{text}{Text to be segmented}

  \item{pat}{The regular expression for recognizing end of
  sentence delimiters.}

  \item{abbreviations}{A list of abbreviations'.' and
  therefore should not be used to segment text}

  \item{stripempty}{Remove empty "sentences", TRUE by
  default.  Should only be set to false if for some reason
  you wanted to preserve the original text with all of its
  spaces etc.}
}
\description{
split a text into sentences This function takes a text and
splits it into sentences.
}
\examples{
test <- "This is a sentence! Several sentences. It's designed by a Dr. to test whether this function works. Or not? Or not."
sentenceSeg(test)
}

