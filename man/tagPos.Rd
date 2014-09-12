\name{tagPos}
\alias{tagPos}
\title{Returns a table of the occurrences of different parts of speech in a sentence
This function takes a sentence and tags each word with it's part of speech using
openNLP's POS tagger, then returns a table of the parts of speech}
\usage{
tagPos(sentence)
}
\arguments{
  \item{sentence}{Sentence to be tagged}
}
\description{
http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
}
\examples{
\dontrun{
tagPos("This is an example sentence with nouns and verbs for tagging.")
}
}

