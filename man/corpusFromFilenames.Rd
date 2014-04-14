\name{corpusFromFilenames}
\alias{corpusFromFilenames}
\title{create a new corpus with attribute-value pairs taken from document headers}
\usage{
corpusFromFilenames(directory, attNames, sep = "_")
}
\arguments{
  \item{directory}{}

  \item{directory}{}
}
\description{
This function takes a directory, reads in all the documents
in that directory and makes a new corpus where the
attributes and values are created from JSON headers in the
documents. The JSON header should be the first line (as
delimited by \\n) in document. For example, a document may
begin as follows: "budgetPosition" : "1.0", "party":"FF"\}
When I presented the supplementary budget to this House
last April....
}
\details{
The directory must contain only documents to be used in the
corpus, and each document must have the same attributes.
}
\examples{
\dontrun{
budgets <- corpusFromHeaders("~/Dropbox/QUANTESS/corpora/withHeader")
}
create a new corpus with attribute-value pairs taken from document filenames
Work in progress
}

