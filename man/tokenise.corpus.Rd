\name{tokenise.corpus}
\alias{tokenise.corpus}
\title{tokenize the corpus and store the results in the corpus}
\usage{
\method{tokenise}{corpus}(corp)
}
\arguments{
  \item{corp}{Corpus to be tokenized}
}
\description{
Tokenize the corpus and store the results as a dfm, along
with counts for the syllables of each token, the number of
sentences per text, and the number of paragraphs per text.
}
\examples{
mycorpus <- corpus(uk2010immig)
mycorpus
tokenise(mycorpus)
mycorpus
}

