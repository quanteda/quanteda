% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nfunctions.R
\name{ntoken}
\alias{ntoken}
\alias{ntype}
\title{Count the number of tokens or types}
\usage{
ntoken(x, ...)

ntype(x, ...)
}
\arguments{
\item{x}{a \pkg{quanteda} \link{tokens} or \link{dfm} object}

\item{...}{additional arguments passed to \code{tokens()}}
}
\value{
\code{ntoken()} returns a named integer vector of the counts of the total
tokens

\code{ntypes()} returns a named integer vector of the counts of the types (unique
tokens) per document.  For \link{dfm} objects, \code{ntype()} will only return the
count of features that occur more than zero times in the dfm.
}
\description{
Get the count of tokens (total features) or types (unique tokens).
}
\examples{
# simple example
txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
toks <- tokens(txt)
ntoken(toks)
ntype(toks)
ntoken(tokens_tolower(toks))  # same
ntype(tokens_tolower(toks))   # fewer types

# with some real texts
toks <- tokens(corpus_subset(data_corpus_inaugural, Year < 1806))
ntoken(tokens(toks, remove_punct = TRUE))
ntype(tokens(toks, remove_punct = TRUE))
ntoken(dfm(toks))
ntype(dfm(toks))
}
