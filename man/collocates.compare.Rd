\name{collocates.compare}
\alias{collocates.compare}
\title{This function create a data frame that can be used for selectFeatures()}
\usage{
collocates.compare(file1, file2, thresh = 1, window, stopwords = FALSE)
}
\arguments{
  \item{file1}{Target text}

  \item{file2}{Reference text}

  \item{thresh}{Threshold for collocates.mx2df.score()}

  \item{window}{Window span for
  collocates.findCollocation()}

  \item{stopwords}{Stopwords removal for dfm()}
}
\value{
cfvm2 Collocatons as data frame
}
\description{
This function create a data frame that can be used for
selectFeatures()
}
\examples{
cfvm <- collocates.compare('Con 2010.txt', 'Con 2010.txt', window = 5, stopwords = TRUE)
}
\author{
Kohei Watanabent
}

