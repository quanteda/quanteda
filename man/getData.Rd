\name{getData}
\alias{getData}
\title{extract the attributes (document-level meta-data) from a corpus}
\usage{
getData(corpus, usenames = TRUE)
}
\arguments{
  \item{corpus}{A corpus object}

  \item{usenames}{If TRUE (default) use the text names as
  names for the rows of the returned data frame}
}
\value{
data frame of the meda-data from the corpus
}
\description{
Extract the document-level meta-data from a corpus as a
data frame
}
\examples{
data(iebudgets)
getData(subset(iebudgets, year==2012))
getData(subset(iebudgets, year==2012), usenames=FALSE)
}

