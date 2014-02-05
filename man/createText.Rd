\name{createText}
\alias{createText}
\title{create text from a string}
\usage{
createText(string, fname, atts = NULL)
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
\dontrun{
Cowen05 <- createText("this is a speech",'Cowen05.txt', atts=cowen.atts)
}
}

