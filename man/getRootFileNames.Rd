\name{getRootFileNames}
\alias{getRootFileNames}
\title{Truncate absolute filepaths to root filenames}
\usage{
  getRootFileNames(longFilenames)
}
\arguments{
  \item{longFilenames}{filenames including a full path with
  directory separators}

  \item{...}{expressions evaluated in the context of
  \code{df} and then fed to \code{\link{order}}}
}
\description{
  This function takes an absolute filepath and returns just
  the document name
}
\examples{
getRootFilnames('/home/paul/documents/libdem09.txt')
}
\keyword{manip}

