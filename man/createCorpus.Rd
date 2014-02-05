\name{createCorpus}
\alias{createCorpus}
\title{create a new corpus - a list containing texts and named attributes
This function takes a text (in the form of a character vectors),
performs some cleanup, and splits the text on whitespace, returning
a dataframe of words and their frequncies}
\usage{
createCorpus(texts, textnames = NULL, attribs = NULL, source = NULL,
  notes = NULL, attribs.labels = NULL)
}
\arguments{
  \item{texts}{Text to be tokenized}

  \item{textnames}{Names to assign to the texts}

  \item{attribs}{A data frame of attributes that can be
  associated with each}
}
\description{
create a new corpus - a list containing texts and named
attributes This function takes a text (in the form of a
character vectors), performs some cleanup, and splits the
text on whitespace, returning a dataframe of words and
their frequncies
}
\examples{
\dontrun{
budgets <- createCorpus(texts, attribs=newattribs)
}
}

