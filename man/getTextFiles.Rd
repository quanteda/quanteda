\name{getTextFiles}
\alias{getTextFiles}
\title{load text files from disk into a vector of character vectors
points to files, reads them into a character vector of the texts
with optional names, default being filenames
returns a named vector of complete, unedited texts}
\usage{
getTextFiles(filenames, textnames = NULL)
}
\arguments{
  \item{filenames}{}
}
\value{
character vector of texts read from disk
}
\description{
load text files from disk into a vector of character
vectors points to files, reads them into a character vector
of the texts with optional names, default being filenames
returns a named vector of complete, unedited texts
}
\examples{
\dontrun{
getTextFiles('/home/paul/documents/libdem09.txt')
}
}

