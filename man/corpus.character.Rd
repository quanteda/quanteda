\name{corpus.character}
\alias{corpus.character}
\title{This function creates a corpus from a character vector (of texts),
adds text-specific variables (which we term "attributes"), along
with optional meta-data and notes.}
\usage{
\method{corpus}{character}(texts, docnames = NULL, attribs = NULL,
  metadata = NULL, notes = NULL, citation = NULL, ...)
}
\arguments{
  \item{texts}{A character vector containgin}

  \item{docnames}{Names to be assigned to the texts,
  defaults to the names of the character vector (if any),
  otherwise assigns "text1", "text2", etc.}

  \item{attribs}{A data frame of attributes that is
  associated with each text.}

  \item{source}{A string specifying the source of the
  texts, used for referencing.}

  \item{notes}{A string containing notes about who created
  the text, warnings, To Dos, etc.}
}
\description{
This function creates a corpus from a character vector (of
texts), adds text-specific variables (which we term
"attributes"), along with optional meta-data and notes.
}
\examples{
summary(budgets)
}

