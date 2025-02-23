% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfm_group.R
\name{dfm_group}
\alias{dfm_group}
\title{Combine documents in a dfm by a grouping variable}
\usage{
dfm_group(
  x,
  groups = docid(x),
  fill = FALSE,
  force = FALSE,
  verbose = quanteda_options("verbose")
)
}
\arguments{
\item{x}{a \link{dfm}}

\item{groups}{grouping variable for sampling, equal in length to the number
of documents. This will be evaluated in the docvars data.frame, so that
docvars may be referred to by name without quoting. This also changes
previous behaviours for \code{groups}. See \code{news(Version >= "3.0", package = "quanteda")} for details.}

\item{fill}{logical; if \code{TRUE} and \code{groups} is a factor, then use all levels
of the factor when forming the new documents of the grouped object.  This
will result in a new "document" with empty content for levels not observed,
but for which an empty document may be needed.  If \code{groups} is a factor of
dates, for instance, then \code{fill = TRUE} ensures that the new object will
consist of one new "document" by date, regardless of whether any documents
previously existed with that date.  Has no effect if the \code{groups}
variable(s) are not factors.}

\item{force}{logical; if \code{TRUE}, group by summing existing counts, even if
the dfm has been weighted.  This can result in invalid sums, such as adding
log counts (when a dfm has been weighted by \code{"logcount"} for instance using
\code{\link[=dfm_weight]{dfm_weight()}}).  Not needed when the term weight schemes "count" and
"prop".}

\item{verbose}{if \code{TRUE} print the number of tokens and documents before and
after the function is applied. The number of tokens does not include paddings.}
}
\value{
\code{dfm_group} returns a \link{dfm} whose documents are equal to
the unique group combinations, and whose cell values are the sums of the
previous values summed by group. Document-level variables that have no
variation within groups are saved in \link{docvars}.  Document-level
variables that are lists are dropped from grouping, even when these exhibit
no variation within groups.
}
\description{
Combine documents in a \link{dfm} by a grouping variable, by summing the cell
frequencies within group and creating new "documents" with the group labels.
}
\examples{
corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
               docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
dfmat <- dfm(tokens(corp))
dfm_group(dfmat, groups = grp)
dfm_group(dfmat, groups = c(1, 1, 2, 2))

# with fill = TRUE
dfm_group(dfmat, fill = TRUE,
          groups = factor(c("A", "A", "B", "C"), levels = LETTERS[1:4]))
}
