\name{addCorpusAttributes}
\alias{addCorpusAttributes}
\title{This function adds a named list of attributes to an existing corpus}
\usage{
addCorpusAttributes(corpus, newattribs, name = newattribs)
}
\arguments{
  \item{corpus}{Corpus to add attributes to}

  \item{newattribs}{A list of new attributes should be a
  named list of length(corpus$texts)}

  \item{name}{A name for the new attribues}
}
\value{
corpus A corpus with the new attributes added
}
\description{
This function adds a named list of attributes to an
existing corpus
}

