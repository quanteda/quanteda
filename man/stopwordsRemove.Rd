\name{stopwordsRemove}
\alias{stopwordsRemove}
\alias{stopwordsRemove.character}
\alias{stopwordsRemove.matrix}
\title{remove stopwords from a text or dfm}
\usage{
stopwordsRemove(text, stopwords = NULL)

\method{stopwordsRemove}{character}(text, stopwords = NULL)

\method{stopwordsRemove}{matrix}(text, stopwords = NULL)
}
\arguments{
  \item{text}{Text from which stopwords will be removed}

  \item{stopwords}{Character vector of stopwords to remove}
}
\value{
a character vector or dfm with stopwords removed
}
\description{
This function takes a character vector or \link{dfm} and
removes words in the list provided in 'stopwords'. If no
list of stopwords is provided a default list for English is
used.
}
\examples{
## examples for character objects
someText <- "Here is an example of text containing some stopwords we want to remove."
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
stopwordsRemove(someText)
stopwordsRemove(someText, stopwordsGet("SMART"))
stopwordsRemove(itText, stopwordsGet("italian"))
stopwordsRemove(someText, c("containing", "example"))

## example for dfm objects
data(iebudgets)
wfm <- dfm(subset(iebudgets, year==2010))
wfm.nostopwords <- stopwordsRemove(wfm)
dim(wfm)
dim(wfm.nostopwords)
dim(stopwordsRemove(wfm, stopwordsGet("SMART")))
}

