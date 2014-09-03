\name{corpus}
\alias{corpus}
\title{Constructor for corpus objects}
\usage{
corpus(texts, ...)
}
\arguments{
  \item{texts}{A character vector. This may be either a
  filepath to a directory containing text documents, or a
  character vector containing texts. file.exists checks if
  it is a valid path}

  \item{docnames}{Names to be assigned to the texts,
  defaults to the names of the character vector (if any),
  otherwise assigns "text1", "text2", etc.}

  \item{attribs}{A data frame of attributes that is
  associated with each text.}

  \item{attribsIn}{Specifies if the attributes and values
  are encoded in file names, directory names, or headers
  values are 'filenames', 'dirnames', or 'headers'}

  \item{sep}{Separator for attribute values if specified in
  file names or directory names}

  \item{source}{A string specifying the source of the
  texts, used for referencing.}

  \item{notes}{A string containing notes about who created
  the text, warnings, To Dos, etc. todo examples}
}
\description{
This function creates a corpus from a character vector (of
texts), adds text-specific variables (which we term
"attributes"), along with optional meta-data and notes.
}

