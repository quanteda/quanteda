\name{kwic2}
\alias{kwic2}
\title{This function is an alternative KWIC}
\usage{
kwic2(texts, word, window = 30, filter = "", location = TRUE,
  case = TRUE)
}
\arguments{
  \item{text}{Texts}

  \item{word}{Word of interest}

  \item{window}{Window span in character}

  \item{filter}{Filter files in texts by regular
  expression}

  \item{location}{Show location of the word}

  \item{case}{Ignore case}
}
\value{
cfvm2 Collocatons as data frame
}
\description{
This function is an alternative KWIC
}
\examples{
\dontrun{
kwic2(texts, "we", filter = '_2010', location=TRUE)
}
}
\author{
Kohei Watanabent
}

