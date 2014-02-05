\name{wordfishMCMC}
\alias{wordfishMCMC}
\title{Bayesian-MCMC version of the "wordfish" Poisson scaling model}
\usage{
wordfishMCMC(dtm, dir = c(1, 2), control = list(sigma = 3, startparams =
  NULL), alphaModel = c("free", "logdoclength", "modelled"),
  alphaFormula = NULL, alphaData = NULL, wordPartition = NULL,
  betaPartition = FALSE, wordConstraints = NULL, verbose = TRUE,
  nChains = 1, nAdapt = 100, nUpdate = 300, nSamples = 100, nThin = 1,
  ...)
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
  in log likelihood sufficient to halt estimatioe,
  \code{sigma} the standard deviation for the beta prior in
  poisson form, and \code{startparams} a previously fitted
  wordfish model.  \code{verbose} generates a running
  commentary during estimation.  See
  \code{austin::wordfish}.}

  \item{alphaModel}{\code{free} means the alpha_i is
  entirely estimated; \code{logdoclength} means the alpha
  is predicted with an expected value equal to the log of
  the document length in words, similar to an offset in a
  Poisson model with variable exposure; \code{modelled}
  allows you to specify a formula and covariates for
  \alpha_i using \code{alphaFormula} and \code{alphaData}.}

  \item{alphaFormula}{Model formula for hierarchical model
  predicting \alpha_i.}

  \item{alphaData}{Data to form the model matrix for the
  hierarchical model predicting \alpha_i.}

  \item{wordPartition}{A vector equal in length to the
  documents that specifies a unique value partitioning the
  word parameters. For example, alpha could be a Boolean
  variable for \code{EU} to indicate that a document came
  from a country outside the EU or inside the EU.  Or, it
  could be a factor variable indicating the name of the
  country (as long as there are multiple documents per
  country).  Internally, \code{wordPartition} is coerced to
  a factor.  \code{NULL} indicates that no paritioning of
  the word-level parameters will take place (default).}

  \item{betaPartition}{Boolean indicating that the \beta
  parameter should also be partitioned according to
  \code{wordPartition}.}

  \item{wordConstraints}{An index with a minimim length of
  1, indicating which words will be set equal across the
  \code{wordPartition} factors.  \code{NULL} if
  \code{is.null(wordPartition)} (default).}

  \item{verbose}{Turn this on for messages.  Default is
  \code{TRUE}.}

  \item{nchains}{Number of chains to run in JAGS.}

  \item{nadapt}{Adaptation iterations in JAGS.}

  \item{nupdate}{Update iterations in JAGS.}

  \item{nsamples}{Number of posterior samples to draw in
  JAGS.}

  \item{nthin}{Thinning parameter for drawing posterior
  samples in JAGS.}

  \item{...}{Additional arguments passed through.}
}
\value{
An augmented \code{wordfish} object with additional stuff
packed in.  To be documented.
}
\description{
\code{wordfishMCMC} implements a flexible, Bayesian model
estimated in JAGS using MCMC. It is based on the
implementation of \code{wordfish} from the \code{austin}
package. Options include specifying a model for alpha using
document-level covariates, and partitioning the word
parameters into different subsets, for instance, countries.
}
\author{
Kenneth Benoit
}

