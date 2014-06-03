\name{stopwords}
\alias{stopwords}
\title{access stopwords}
\usage{
stopwords(kind = "english")
}
\arguments{
  \item{text}{Text from which stopwords will be removed}

  \item{stopwords}{Character vector of stopwords to remove}
}
\value{
a character vector or dfm with stopwords removed
}
\description{
This function retrieves stopwords from the type specified
in the \code{kind} argument and returns the stopword list
as a character vector The default is English.
}
\examples{
someText <- "Here is an example of text containing some stopwords we want to remove. "
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
removeStopwords(someText)
removeStopwords(someText, kind="italian")
removeStopwords(someText, stopwords = c("containing", "example"))
}

