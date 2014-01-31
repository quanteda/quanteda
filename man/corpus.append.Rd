\name{corpus.append}
\alias{corpus.append}
\title{function to add new texts and attributes to an existing corpus
Accepts a list of texts and a list of associated attributes and
adds them to the corpus}
\usage{
corpus.append(corpus1, newtexts, newattribs, ...)
}
\arguments{
  \item{corpus}{An existing corpus to add new texts and
  attributes to}

  \item{newtexts}{New texts to be added to the corpus}

  \item{newattribs}{New attribs associated with the new
  texts text}
}
\description{
function to add new texts and attributes to an existing
corpus Accepts a list of texts and a list of associated
attributes and adds them to the corpus
}
\examples{
budgets <- corpus.append(budgets, texts, newattribs)
}

