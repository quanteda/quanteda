\name{index.corpus}
\alias{index.corpus}
\title{index the tokens in a corpus}
\usage{
index.corpus(corp)
}
\arguments{
  \item{corp}{Corpus to be indexed}
}
\value{
no return but modifies the object in place by changing

\item{tokens, }{a list consisting of the following:}

\item{$dfm}{A \link{dfm} document-feature matrix object
created with \link{settings}.}

\item{$nwords}{A vector of token counts for each document.}

\item{$ntypes}{A vector of type counts for each document.}

\item{$nsents}{A vector of sentence counts for each
document.}

\item{$nparagr}{A vector of paragraph counts for each
document.}
}
\description{
Applies pre-processing rules to the text and compiles a
frequency table of features (word types) including counts
of types, tokens, sentences, and paragraphs.
}
\note{
This is nothing close to a true "index" but will eventually
become a better indexing function.  At the moment it
creates and saves a \link{dfm} in addition to some summary
information compiled from this, in order to speed up
subsequent processing. Unlike most R functions which return
a value, this one changes the object passed to it.  (And
they say R can't pass by reference...)
}
\examples{
mycorpus <- corpus(uk2010immig)
mycorpus
index(mycorpus)
mycorpus
}

