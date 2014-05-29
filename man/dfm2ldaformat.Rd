\name{dfm2ldaformat}
\alias{dfm2ldaformat}
\title{Convert a quanteda \code{\link{dfm}} (document feature matrix)
 into a the data format needed by \link[pkg]{lda}}
\usage{
dfm2ldaformat(d)
}
\arguments{
  \item{d}{A \code{\link{dfm}} object}
}
\value{
A list with components "documents" and "vocab" as needed by
\link[pkg]{lda.collapsed.gibbs.sampler}
}
\description{
Convert a quanteda \code{\link{dfm}} (document feature
matrix) into a the data format needed by \link[pkg]{lda}
}
\examples{
\dontrun{
data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
# create document-feature matrix, remove stopwords
d <- dfm(iebudgets2010, stopwords=TRUE)
# trim low frequency words
d <- dfmTrim(d, minCount=5, minDoc=3)
td <- dfm2ldaformat(d)
if (require(lda)) {
    tmodel.lda <- result <- lda.collapsed.gibbs.sampler(documents=td$documents,
                                                        K=10,
                                                        vocab=td$vocab,
                                                        num.iterations=50, alpha=0.1, eta=0.1)
}
top.topic.words(tmodel.lda$topics, 10, by.score=TRUE) # top five words in each topic
}
}

