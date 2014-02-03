\name{removeStopwords}
\alias{removeStopwords}
\title{remove common or 'semantically empty' words from a text.
This function takes a character vector 'text' and removes words in the
list provided in 'stopwords'. If no list of stopwords is provided a
default list for English is used.
openNLP's POS tagger, then returns a table of the parts of speech}
\usage{
removeStopwords(text, stopwords = NULL)
}
\arguments{
  \item{text}{Text to alter}

  \item{stopwords}{List of stopwords to remove}
}
\description{
http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
}
\examples{
removeStopwords(someText)
}

