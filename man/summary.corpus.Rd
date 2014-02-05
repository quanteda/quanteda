\name{summary.corpus}
\alias{summary.corpus}
\title{Display a summary of a corpus object
Displays information about a corpus object, including attributes and
metadata such as date of number of texts, creation and source}
\usage{
\method{summary}{corpus}(corpus, texts = "texts", subset = NULL,
  select = NULL, drop = FALSE, output = TRUE, nmax = 100)
}
\arguments{
  \item{corpus}{An existing corpus to be summarized}

  \item{texts}{The name of the attribute containing the
  corpus texts, if not 'texts'}
}
\description{
Display a summary of a corpus object Displays information
about a corpus object, including attributes and metadata
such as date of number of texts, creation and source
}
\examples{
\dontrun{
summary.corpus(corpus1)
}
}

