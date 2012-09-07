\name{corpus.create}
\alias{corpus.create}
\title{create a new corpus - a list containing texts and named attributes
This function takes a text (in the form of a character vectors),
performs some cleanup, and splits the text on whitespace, returning
a dataframe of words and their frequncies}
\usage{
  corpus.create(texts, textnames = NULL, attribs = NULL,
    source = NULL, notes = NULL, attribs.labels = NULL)
}
\arguments{
  \item{texts}{Text to be tokenized}

  \item{textnames}{Names to assign to the texts}

  \item{attribs}{A data frame of attributes that can be
  associated with each text}
}
\description{
  create a new corpus - a list containing texts and named
  attributes This function takes a text (in the form of a
  character vectors), performs some cleanup, and splits the
  text on whitespace, returning a dataframe of words and
  their frequncies
}
\examples{
budgets <- corpus.create(texts, attribs=newattribs)
}

