\name{getRootFileNames}
\alias{getRootFileNames}
\title{Truncate absolute filepaths to root filenames}
\usage{
getRootFileNames(longFilenames)
}
\arguments{
  \item{longFilenames}{Absolute filenames including a full
  path with directory}
}
\value{
character vector of filenames withouth directory path
}
\description{
This function takes an absolute filepath and returns just
the document name
}
\examples{
getRootFileNames('/home/paul/documents/libdem09.txt')
}

