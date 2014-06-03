<<<<<<< HEAD
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
=======
\docType{data}
\name{stopwords}
\alias{stopwords}
\title{A named list containing common stopwords in 14 languages}
\description{
SMART English stopwords from the SMART information
retrieval system (obtained from
http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop)
and a set of stopword lists from the Snowball stemmer
project in different languages (obtained from
http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt).
Supported languages are danish, dutch, english, finnish,
french, german, hungarian, italian, norwegian, portuguese,
russian, spanish, and swedish. Language names are case
sensitive. Alternatively, their IETF language tags may be
used.
>>>>>>> 304330e5366a2c59f63041efbb1b458948968ddf
}

