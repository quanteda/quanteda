% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readtext-methods.R
\name{readtext-methods}
\alias{readtext-methods}
\title{Extensions for readtext objects}
\arguments{
\item{x}{an object returned by \code{spacy_parse}, or (for
\code{spacy_parse}) a \link{corpus} object}

\item{...}{not used for these functions}
}
\description{
These functions provide \pkg{quanteda} methods for \pkg{readtext} objects.
}
\details{
\code{texts(x)} returns the texts from a readtext object

\code{docnames(x)} returns the document names from a readtext object

\code{docvars(x, field = NULL)} returns a data.frame of the document variables
from a readtext object

\code{ndoc(x)} returns the number of documents from a readtext object
}
