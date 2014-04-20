\name{tokenize}
\alias{tokenize}
\title{Split a string into words
The input text is split into words by whitespace}
\usage{
tokenize(str, lower = TRUE)
}
\arguments{
  \item{str}{String to be tokenized}
}
\value{
a character vector containing the input text cleaned and
split by whitespace
}
\description{
Split a string into words The input text is split into
words by whitespace
}
\examples{
tokens <- tokenize("this is a test")
}

