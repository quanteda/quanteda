\name{wordcloud.dfm}
\alias{wordcloud.dfm}
\title{Plot a word cloud for a \link{dfm}}
\usage{
\method{wordcloud}{dfm}(dfm, doc.index, ...)
}
\arguments{
  \item{dfm}{document-feature matrix created in quanteda}

  \item{document}{index of the document whose words will be
  plotted}

  \item{...}{additional arguments to pass to
  \link[wordcloud]{wordcloud}}
}
\value{
None
}
\description{
plots a document as a wordcloud of its features
}
\examples{
data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
wfm <- dfm(iebudgets2010, stopwords=TRUE)
wordcloudDfm(wfm, 1)  # plot the finance minister's speech as a wordcloud
}
\author{
Kenneth Benoit
}

