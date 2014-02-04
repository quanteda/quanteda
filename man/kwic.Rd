\name{kwic}
\alias{kwic}
\title{List key words in context from a text or a corpus of texts.}
\usage{
kwic(text, word, window = 5)
}
\arguments{
  \item{text}{A text character scalar or a quanteda corpus.
  (Currently does not support character vectors.)}

  \item{word}{A keyword chosen by the user.}

  \item{window}{The number of context words to be displayed
  around the keyword.}
}
\value{
A data frame with the context before (\code{preword}), the
keyword in its original format (\code{word}, preserving
case and attached punctuation), and the context after
(\code{postword}).  The rows of the dataframe will be named
with the word index position, or the text name and the
index position for a corpus object.
}
\description{
For a text or a collection of texts (in a quanteda corpus
object), return a list of a keyword supplied by the user in
its immediate context, identifying the source text and the
word index number within the source text.  (Not the line
number, since the text may or may not be segmented using
end-of-line delimiters.)
}
\examples{
data(iebudgets)
kwic(iebudgets, "Christmas", window=6)
}
\author{
Kenneth Benoit and Paul Nulty
}

