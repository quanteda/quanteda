\name{translate}
\alias{translate}
\title{Send text to the google translate research API
This function translates a text by sending it to the google translate API.}
\usage{
translate(sourceText, sourceLanguage, targetLanguage, key = NULL,
  verbose = FALSE)
}
\arguments{
  \item{sourceText}{Text to be translated}

  \item{sourceLanguage}{Language of the source text}

  \item{targetLanguage}{Language of the translated text}

  \item{key}{API key for Google Translate research API}
}
\description{
Send text to the google translate research API This
function translates a text by sending it to the google
translate API.
}
\examples{
\dontrun{translation <- translate(original, fr, de, key='insertkeyhere')}
}

