\name{create.text}
\alias{create.text}
\title{create text from a string}
\usage{
create.text(string, fname, atts = NULL)
}
\arguments{
  \item{text}{The string of text}

  \item{fname}{The name of the file containing the text}

  \item{atts}{A data frame of attributes that can be
  associated with each text}
}
\description{
This function associates a string of text with a list of
attribute:value pairs
}
\examples{
Cowen05 <- create.text("this is a speech",'Cowen05.txt', atts=cowen.atts)
}

