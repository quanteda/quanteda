\name{removeStopwords}
\alias{removeStopwords}
\title{remove common or 'semantically empty' words from a text.
This function takes a character vector 'text' and removes words in the
list provided in 'stopwords'. If no list of stopwords is provided a
default list for English is used.}
\usage{
removeStopwords(text, stopwords = NULL)
}
\arguments{
  \item{text}{Text to alter}

  \item{stopwords}{List of stopwords to remove}
}
\value{
a character vector of text with stopwords removed
}
\description{
remove common or 'semantically empty' words from a text.
This function takes a character vector 'text' and removes
words in the list provided in 'stopwords'. If no list of
stopwords is provided a default list for English is used.
}
\examples{
someText <- "This text has some stopwords we want to remove"
removeStopwords(someText)
}

