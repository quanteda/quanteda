\name{corpusFromHeaders}
\alias{corpusFromHeaders}
\title{create a new corpus with attribute-value pairs taken from document headers}
\usage{
corpusFromHeaders(headerTexts)
}
\arguments{
  \item{headerTexts}{A vector of texts with JSON headers}
}
\description{
This function takes a vector of texts with JSON headers and
makes a new corpus where the attributes and values are
created from JSON headers in the text. The JSON header
should be the first line (as delimited by \\n) in document.
For example, a document may begin as follows:
"budgetPosition" : "1.0", "party":"FF"\} When I presented
the supplementary budget to this House last April....
}
\details{
The directory must contain only documents to be used in the
corpus, and each document must have the same attributes.
}
\examples{
data(ieTextsHeaders)
budgets <- corpusFromHeaders(ieTextsHeaders)
}

