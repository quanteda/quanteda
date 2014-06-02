\name{removeStopwords}
\alias{removeStopwords}
\title{remove common or 'semantically empty' words from a text.
This function takes a character vector 'text' and removes words in the
list provided in 'stopwords'. If no list of stopwords is provided a
default list for English is used.}
\usage{
removeStopwords(text, kind = "english", stopwords = NULL)
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
someText <- "Here is an example of text containing some stopwords we want to remove. "
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
removeStopwords(someText)
removeStopwords(someText, kind="italian")
removeStopwords(someText, stopwords = c("containing", "example"))
}

