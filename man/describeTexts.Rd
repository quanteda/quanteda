\name{describeTexts}
\alias{describeTexts}
\title{print a summary of texts
Prints to the console a desription of the texts, including
number of types, tokens, and sentences}
\usage{
describeTexts(txts, verbose = TRUE)
}
\arguments{
  \item{txts}{The texts to be described}

  \item{verbose}{Default is TRUE. Set to false to suppress
  output messages}
}
\description{
print a summary of texts Prints to the console a desription
of the texts, including number of types, tokens, and
sentences
}
\examples{
describeTexts(c("testing this text", "and this one"))
describeTexts(uk2010immig)
}

