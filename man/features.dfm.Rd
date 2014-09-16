\name{features.dfm}
\alias{features}
\alias{features.dfm}
\title{extract the feature labels from a \link{dfm}}
\usage{
\method{features}{dfm}(x)
}
\value{
Character vector of the features
}
\description{
Extract the features from a document-feature matrix, which
are stored as the column names of the \link{dfm} object.
}
\examples{
features(dfm(inaugTexts))[1:50]  # first 50 features (alphabetically sorted)
}

