% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfm_subset.R
\name{dfm_subset}
\alias{dfm_subset}
\title{Extract a subset of a dfm}
\usage{
dfm_subset(
  x,
  subset,
  min_ntoken = NULL,
  max_ntoken = NULL,
  drop_docid = TRUE,
  ...
)
}
\arguments{
\item{x}{\link{dfm} object to be subsetted.}

\item{subset}{logical expression indicating the documents to keep: missing
values are taken as false.}

\item{min_ntoken, max_ntoken}{minimum and maximum lengths of the documents to extract.}

\item{drop_docid}{if \code{TRUE}, \code{docid} for documents are removed as the result
of subsetting.}

\item{...}{not used}
}
\value{
\link{dfm} object, with a subset of documents (and docvars) selected
according to arguments
}
\description{
Returns document subsets of a dfm that meet certain conditions,
including direct logical operations on docvars (document-level variables).
\code{dfm_subset} functions identically to \code{\link[=subset.data.frame]{subset.data.frame()}},
using non-standard evaluation to evaluate conditions based on the
\link{docvars} in the dfm.
}
\details{
To select or subset \emph{features}, see \code{\link[=dfm_select]{dfm_select()}} instead.

When \code{select} is a dfm, then the returned dfm will be equal in
document dimension and order to the dfm used for selection.  This is the
document-level version of using \code{\link[=dfm_select]{dfm_select()}} where
\code{pattern} is a dfm: that function matches features, while
\code{dfm_subset} will match documents.
}
\examples{
corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                 d3 = "b b c e", d4 = "e e f a b"),
               docvars = data.frame(grp = c(1, 1, 2, 3)))
dfmat <- dfm(tokens(corp))
# selecting on a docvars condition
dfm_subset(dfmat, grp > 1)
# selecting on a supplied vector
dfm_subset(dfmat, c(TRUE, FALSE, TRUE, FALSE))
}
\seealso{
\code{\link[=subset.data.frame]{subset.data.frame()}}
}
\keyword{dfm}
