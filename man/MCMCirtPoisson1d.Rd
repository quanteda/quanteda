\name{MCMCirtPoisson1d}
\alias{MCMCirtPoisson1d}
\title{Bayesian-MCMC version of a 1-dimensional Poisson IRT scaling model}
\usage{
MCMCirtPoisson1d(dtm, dir = c(1, 2), control = list(sigma = 3, startparams =
  NULL), verbose = TRUE, itembase = NULL, nChains = 1, nAdapt = 100,
  nUpdate = 300, nSamples = 200, nThin = 1, ...)
}
\arguments{
  \item{dtm}{The document-term matrix.  Ideally, documents
  form the rows of this matrix and words the columns,
  although it should be correctly coerced into the correct
  shape.}

  \item{dir}{A two-element vector, enforcing direction
  constraints on theta and beta, which ensure that
  theta[dir[1]] < theta[dir[2]]. The elements of \code{dir}
  will index documents.}

  \item{control}{list specifies options for the estimation
  process. These are: \code{tol}, the proportional change
  in log likelihood sufficient to halt estimation,
  \code{sigma} the standard deviation for the beta prior in
  poisson form, and \code{startparams} a previously fitted
  wordfish model.  \code{verbose} generates a running
  commentary during estimation.  See
  \code{austin::wordfish}.}

  \item{itembase}{A index or column name from \code{dtm}
  indicating which item parameters should be used as the
  reference categories. (These will have \eqn{\beta_j=0}
  and \eqn{\alpha_j=0}.)  If set to NULL (default) then no
  constraints will be implemented.  See details.}

  \item{verbose}{Turn this on for messages.  Default is
  \code{TRUE}.}

  \item{nChains}{Number of chains to run in JAGS.}

  \item{nAdapt}{Adaptation iterations in JAGS.}

  \item{nUpdate}{Update iterations in JAGS.}

  \item{nSamples}{Number of posterior samples to draw in
  JAGS.}

  \item{nThin}{Thinning parameter for drawing posterior
  samples in JAGS.}

  \item{...}{Additional arguments passed through.}
}
\value{
An augmented \code{wordfish} class object with additional
stuff packed in.  To be documented.
}
\description{
\code{MCMCirtPoisson1d} implements a flexible, Bayesian
model estimated in JAGS using MCMC. It is based on the
implementation of \code{wordfish} from the \code{austin}
package. Options include specifying a model for alpha using
document-level covariates, and partitioning the word
parameters into different subsets, for instance, countries.
}
\details{
The ability to constrain an item is designed to make the
additive Poisson GLM mathematically equivalent to the
multinomial model for \eqn{R \times C} contingency tables.
We recommend setting a neutral category to have
\eqn{\psi_{0}=0 and \beta_{0}=0}, for example the word
"the" for a text count model (assuming this word has not
been removed).  Note: Currently the item-level return
values will be returned in the original order suppled
(\code{psi} and \cde{beta}) but this is not true yet for
the \code{mcmc.samples} value, which will have the
constrained category as index 1.  (We will fix this soon.)
}
\examples{
\dontrun{
data(iebudgets)
# extract just the 2010 debates
iebudgets2010 <- subset(iebudgets, year==2010)

# create a document-term matrix and set the word margin to the columns
dtm <- dfm(iebudgets2010)

# estimate the maximium likelihood wordfish model from austin
require(austin)
iebudgets2010_wordfish <- wordfish(as.wfm(dtm, word.margin=2), dir=c(2,1))

# estimate the MCMC model, default values
iebudgets2010_wordfishMCMC <- MCMCirtPoisson1d(dtm, itembase="the", dir=c(2,1))
iebudgets2010_wordfishMCMC_unconstrained <- MCMCirtPoisson1d(dtm, dir=c(2,1))

# compare the estimates of \\eqn{\\theta_i}
require(psych)
pairs.panels(data.frame(ML=iebudgets2010_wordfish$theta,
                        PoissonThe=iebudgets2010_wordfishMCMC$theta,
                        PoissonUnconst=iebudgets2010_wordfishMCMC_unconstrained$theta),
             smooth=FALSE, scale=FALSE, ellipses=FALSE, lm=TRUE, cex.cor=2.5)
# inspect a known "opposition" word beta values
iebudgets2010_wordfish$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
iebudgets2010_wordfishMCMC$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
iebudgets2010_wordfishMCMC_unconstrained$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
}
}
\author{
Kenneth Benoit
}

