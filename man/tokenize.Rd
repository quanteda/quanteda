\name{tokenize}
\alias{tokenize}
\title{split a text into words and return a table of words and their counts
This function takes a text (in the form of a character vectors),
performs some cleanup, and splits the text on whitespace, returning
a dataframe of words and their frequncies}
\usage{
  tokenize(text, textname = "count")
}
\arguments{
  \item{text}{Text to be tokenized}
}
\description{
  split a text into words and return a table of words and
  their counts This function takes a text (in the form of a
  character vectors), performs some cleanup, and splits the
  text on whitespace, returning a dataframe of words and
  their frequncies
}
\examples{
tokenize("This is an example sentence.")
}

