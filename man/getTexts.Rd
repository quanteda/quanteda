\name{getTexts}
\alias{getTexts}
\title{extract the texts from a corpus}
\usage{
getTexts(corpus, usenames = TRUE)
}
\arguments{
  \item{corpus}{A corpus object}

  \item{usenames}{If TRUE (default) use the text names as
  names for the returned character vector.}
}
\value{
(Named) character vector of texts from the corpus.
}
\description{
Extract the texts from a corpus as a named character vector
}
\examples{
data(iebudgets)
cowenTexts <- getTexts(subset(iebudgets, speaker="Cowen"))
syllableCount(cowenTexts)
}

