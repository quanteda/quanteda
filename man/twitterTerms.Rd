\name{twitterTerms}
\alias{twitterTerms}
\title{make a corpus object from results of a twitter search
All of the attributes returned by the twitteR
library call are included as attributes in the
corpus. A oauth key is required, for further
instruction about the oauth processs see:
https://dev.twitter.com/apps/new
and the twitteR documentation}
\usage{
twitterTerms(query, oauth, numResults = 50)
}
\arguments{
  \item{query}{Search string for twitter}

  \item{oauth}{Oauth key}

  \item{numResults}{Number of results desired.}
}
\description{
make a corpus object from results of a twitter search All
of the attributes returned by the twitteR library call are
included as attributes in the corpus. A oauth key is
required, for further instruction about the oauth processs
see: https://dev.twitter.com/apps/new and the twitteR
documentation
}
\examples{
twCorp <- twitterTerms('example search', numResults=10)
}

