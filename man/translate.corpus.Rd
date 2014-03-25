\name{translate.corpus}
\alias{translate.corpus}
\title{Send a corpus to the google translate research API
This function translates a the texts in a corpus by sending them
 to the google translate API.}
\usage{
translate.corpus(corpus, targetlanguageString, textvar = "texts",
  languagevar = "language", key = NULL)
}
\arguments{
  \item{corpus}{corpus to be translated}

  \item{targetlanguageString}{Language of the source text}

  \item{languagevar}{Language of the translated text}
}
\description{
Send a corpus to the google translate research API This
function translates a the texts in a corpus by sending them
to the google translate API.
}
\examples{
\dontrun{
translation <- translate(original, fr, de, key='insertkeyhere')
}
}

