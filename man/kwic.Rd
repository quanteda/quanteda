\name{kwic}
\alias{kwic}
\title{return the context in which 'word' appears
Keyword-in-context function to return the
context in which 'word' appears}
\usage{
kwic(text, word, window = 5)
}
\arguments{
  \item{corpus}{The corpus to be searched}

  \item{word}{The keyword to look for}

  \item{window}{The size of the context window (the number
  of words each side of keyword to return) the default is
  5.}
}
\description{
return the context in which 'word' appears
Keyword-in-context function to return the context in which
'word' appears
}
\examples{
contexts <- kwic(corpus, keyword, window=6)
}

