\name{removeStopwords}
\alias{removeStopwords}
\alias{removeStopwords.character}
\alias{removeStopwords.matrix}
\title{remove stopwords from a text or dfm}
\usage{
removeStopwords(text, stopwords)

\method{removeStopwords}{character}(text, stopwords = NULL)

\method{removeStopwords}{matrix}(dfmObject, stopwords = NULL)
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
someText <- "Here is an example of text containing some stopwords we want to remove. "
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
removeStopwords(someText)
removeStopwords(someText, getStopwords("SMART"))
removeStopwrods(itText, getStopwords("italian"))
removeStopwords(someText, c("containing", "example"))
data(iebudgets)
wfm <- dfm(subset(iebudgets, year==2010))
wfm.nostopwords <- removeStopwords(wfm)
wfm.nostopwords <- removeStopwords(wfm, getStopwords("SMART"))
}

