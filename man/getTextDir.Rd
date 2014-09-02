\name{getTextDir}
\alias{getTextDir}
\title{loads all text files from a given directory}
\usage{
getTextDir(dirname, enc = "detect")
}
\arguments{
  \item{dirname}{A directory path}
}
\value{
character vector of texts read from disk
}
\description{
given a directory name, get a list of all files in that
directory and load them into a character vector using
getTextFiles
}
\examples{
\dontrun{
getTextDir('/home/paul/documents/')
}
}
\author{
Paul Nulty
}

