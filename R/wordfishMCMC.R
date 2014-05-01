##' Bayesian-MCMC version of the "wordfish" Poisson scaling model
##'
##' \code{wordfishMCMC} implements a flexible, Bayesian model estimated in JAGS using MCMC.  
##' It is based on the implementation of \code{wordfish} from the \code{austin} package.
##' Options include specifying a model for alpha using document-level covariates, 
##' and partitioning the word parameters into different subsets, for instance, countries.
##' 
##' @export
##' @param dtm The document-term matrix.  Ideally, documents form the rows of this matrix and words the columns, 
##' although it should be correctly coerced into the correct shape.
##' @param dir A two-element vector, enforcing direction constraints on theta and beta, which ensure that theta[dir[1]] < theta[dir[2]].
##' The elements of \code{dir} will index documents.
##' @param control list specifies options for the estimation process. These are: \code{tol}, the proportional change in log likelihood
##' sufficient to halt estimatioe, \code{sigma} the standard deviation for the beta prior in poisson form, and \code{startparams} a
##' previously fitted wordfish model.  \code{verbose} generates a running commentary during estimation.  See \code{austin::wordfish}.
##' @param alphaModel \code{free} means the \eqn{\alpha_i} is entirely estimated; \code{logdoclength} means the alpha is predicted with an 
##' expected value equal to the log of the document length in words, similar to an offset in a Poisson model with variable exposure;
##' \code{modelled} allows you to specify a formula and covariates for \eqn{\alpha_i} using \code{alphaFormula} and \code{alphaData}.
##' @param alphaFormula Model formula for hierarchical model predicting \eqn{\alpha_i}.
##' @param alphaData Data to form the model matrix for the hierarchical model predicting \eqn{\alpha_i}.
##' @param wordPartition A vector equal in length to the documents that specifies a unique value partitioning the word parameters.  
##' For example, alpha could be a Boolean variable for \code{EU} to indicate that a document came from a country outside the EU or 
##' inside the EU.  Or, it could be a factor variable indicating the name of the country (as long as there are multiple documents
##' per country).  Internally, \code{wordPartition} is coerced to a factor.  \code{NULL} indicates that no paritioning of the 
##' word-level parameters will take place (default).
##' @param betaPartition Boolean indicating that the \eqn{\beta} parameter should also be partitioned according to \code{wordPartition}.
##' @param wordConstraints An index with a minimim length of 1, indicating which words will be set equal across 
##' the \code{wordPartition} factors.  \code{NULL} if \code{is.null(wordPartition)} (default).
##' @param verbose Turn this on for messages.  Default is \code{TRUE}.
##' @param nChains Number of chains to run in JAGS.
##' @param nAdapt Adaptation iterations in JAGS.
##' @param nUpdate Update iterations in JAGS.
##' @param nSamples Number of posterior samples to draw in JAGS.
##' @param nThin Thinning parameter for drawing posterior samples in JAGS.
##' @param ... Additional arguments passed through.
##' @return An augmented \code{wordfish} class object with additional stuff packed in.  To be documented.
##' @author Kenneth Benoit
##' @examples
##' \dontrun{
##' data(iebudgets)
##' # extract just the 2010 debates
##' iebudgets2010 <- corpus.subset(iebudgets, year==2010)
##' 
##' # create a document-term matrix and set the word margin to the columns
##' dtm <- create.fvm.corpus(iebudgets2010)
##' dtm <- wfm(t(dtm), word.margin=2)
##' 
##' # estimate the maximium likelihood wordfish model from austin
##' iebudgets2010_wordfish <- wordfish(dtm, dir=c(2,1))
##' 
##' # estimate the MCMC model, default values
##' iebudgets2010_wordfishMCMC <- wordfishMCMC(dtm, dir=c(2,1))
##' 
##' # compare the estimates of \eqn{\theta_i}
##' plot(iebudgets2010_wordfish$theta, iebudgets2010_wordfishMCMC$theta)
##' 
##' # MCMC with a partition of the word parameters according to govt and opposition
##' # (FF and Greens were in government in during the debate over the 2010 budget)
##' # set the constraint on word partitioned parameters to be the same for "the" and "and"
##' iebudgets2010_wordfishMCMC_govtopp <- 
##'     wordfishMCMC(dtm, dir=c(2,1), 
##'     wordPartition=(iebudgets2010$attribs$party=="FF" | iebudgets2010$attribs$party=="Green"),
##'     betaPartition=TRUE, wordConstraints=which(words(dtm)=="the"))
##' }
wordfishMCMC <- function(dtm, dir=c(1,2), control=list(sigma=3, startparams=NULL),
                         alphaModel=c("free", "logdoclength", "modelled"),
                         alphaFormula=NULL, alphaData=NULL, 
                         wordPartition=NULL, betaPartition=FALSE, wordConstraints=NULL,
                         verbose=TRUE, 
                         nChains=1, nAdapt=100, nUpdate=300, nSamples=100, nThin=1,
                         ...) {
    ## record the function call, start time
    thecall <- match.call()
    start.time <- proc.time()
    
    ## make sure the object is documents by words  
    if (!is.wfm(dtm)) dtm <- wfm(dtm, word.margin=2) # rows are documents, columns are "words"
    if (wordmargin(dtm)==1) dtm <- as.wfm(t(dtm))    # coerce to docs by words
    
    
    ## the alpha estimation approach - take default argument or user supplied value
    alphaModel <- match.arg(alphaModel) 
    # if (!(alpha %in% c("free", "logdoclength", "modelled"))) stop("Alpha must be one of free, logdoclength, or  modelled")
    if (alphaModel=="modelled" & (is.null(alphaFormula) | is.null(alphaData))) {
        stop("You must supply a formula and data with the alphaModel=modelled option.")  
    }
    
    ## wordPartition is a vector with length equal to the number of documents, with factor values
    # partitioning the words into discrete groups (e.g. countries)
    if (!is.null(wordPartition)) {
        # ensure that wordPartition is an index from 1...C
        if (min(wordPartition) < 0) stop("wordPartition index cannot take negative values.")
        wordPartition <- as.numeric(as.factor(wordPartition))
        # reorder the dtm to make the first words the ones with equality constraints
        if (is.null(wordConstraints)) stop("You must specify wordConstraints with wordPartition of psi.")
        dtm <- dtm[, c(wordConstraints, (1:ncol(dtm))[-wordConstraints])]
        # record the original index of columns, so they can be put back later  
        index <- 1:ncol(dtm)
        original.word.index <- c(index[wordConstraints], index[-wordConstraints])
    }
    
    ## starting values, three options
    # 1) if null, then estimate an ML wordfish model and use those
    # 2) if a wordfish object, extract quantities for starting vals
    # 3) if a list of starting values, use those
    if  (is.null(control$startparams)) {
        if (verbose) cat("Calculating ML wordfish for starting values.\n")
        wordfish.ml <- wordfish(dtm, dir)
        starting.values <- list(raw.theta=wordfish.ml$theta,
                                raw.beta=wordfish.ml$beta,
                                psi=wordfish.ml$psi,
                                alpha=wordfish.ml$alpha)
    } else if ("wordfish" %in% class(control$startparams)) {
        cat("Found a wordfish object for starting values.\n")
        wordfish.ml <- control$startparams
        starting.values <- list(raw.theta=wordfish.ml$theta,
                                raw.beta=wordfish.ml$beta,
                                psi=wordfish.ml$psi,
                                alpha=wordfish.ml$alpha)
    } else starting.values <- control$startparams

    ##
    ## model definitions are moved to outside of this function
    ##
    
    require(rjags)
    load.module("glm")

    if (verbose) {
        cat("\nStarting JAGS run: ")
        cat(paste(nChains, paste("chain", ifelse(nChains>1, "s,", ","), sep=""),
                  nAdapt ,"adaptation cycles,", nUpdate, "updates,", nSamples, "samples.\n\n"))
    }

    ## Initialize the model
    
    ## no word partition models
    if (alphaModel=="free" & is.null(wordPartition)) {
        jags.mod <- jags.model(textConnection(wfmodel_noWordPartition_alphaFree), 
                               data=list(Y=dtm, dir=dir), 
                               inits=starting.values, nChains, nAdapt)
    } 
    if (alphaModel=="logdoclength" & is.null(wordPartition)) {
        # remove alpha as a node which requires a starting value
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        jags.mod <- jags.model(textConnection(wfmodel_noWordPartition_alphaLogDocLength), 
                               data=list(Y=dtm, dir=dir, Ni=apply(dtm, 1, sum)), 
                               inits=starting.values, nChains, nAdapt)
    }
    if (alphaModel=="modelled" & is.null(wordPartition)) {
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        X.alpha <- model.matrix(alphaFormula, data=alphaData)#[, -1, drop=FALSE] # remove the intercept
        # use coefs from regression of median across 56 counts on xs as inits for doc-level intercepts
        median.length <- round(apply(dtm, 1, median))
        startval.alpha <- glm.fit(X.alpha, median.length, family=poisson(link="log"))
        # remove alpha as a node which requires a starting value
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        # add coefficients for alpha as starting values
        starting.values$coef.alpha <- as.numeric(coef(startval.alpha))
        # print(X.alpha)
        jags.mod <- jags.model(textConnection(wfmodel_noWordPartition_alphaModelled),
                               data=list(Y=dtm, dir=dir, X.alpha=X.alpha),
                               inits=starting.values, nChains, nAdapt)
    }

    ## word partition, psi-only models
    if (alphaModel=="free" & !is.null(wordPartition) & !betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        jags.mod <- jags.model(textConnection(wfmodel_psiWordPartition_alphaFree),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints),
                               inits=starting.values, nChains, nAdapt)
    }
    if (alphaModel=="logdoclength" & !is.null(wordPartition) & !betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        jags.mod <- jags.model(textConnection(wfmodel_psiWordPartition_alphaLogDocLength),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints, 
                                         Ni=apply(dtm, 1, sum)),
                               inits=starting.values, nChains, nAdapt)
    }
    if (alphaModel=="modelled" & !is.null(wordPartition) & !betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        X.alpha <- model.matrix(alphaFormula, data=alphaData)
        # use coefs from regression of median across 56 counts on xs as inits for doc-level intercepts
        median.length <- round(apply(dtm, 1, median))
        startval.alpha <- glm.fit(X.alpha, median.length, family=poisson(link="log"))
        # remove alpha as a node which requires a starting value
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        # add coefficients for alpha as starting values
        starting.values$coef.alpha <- as.numeric(coef(startval.alpha))
        jags.mod <- jags.model(textConnection(wfmodel_psiWordPartition_alphaModelled),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints, 
                                         X.alpha=X.alpha),
                               inits=starting.values, nChains, nAdapt)
    }
    
    ## word partition, psi and beta models
    if (alphaModel=="free" & !is.null(wordPartition) & betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        starting.values$raw.beta <- matrix(rep(wordfish.ml$beta, max(wordPartition)), ncol=max(wordPartition))
        jags.mod <- jags.model(textConnection(wfmodel_psibetaWordPartition_alphaFree),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints),
                               inits=starting.values, nChains, nAdapt)
    }
    if (alphaModel=="logdoclength" & !is.null(wordPartition) & betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        starting.values$raw.beta <- matrix(rep(wordfish.ml$beta, max(wordPartition)), ncol=max(wordPartition))
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        jags.mod <- jags.model(textConnection(wfmodel_psibetaWordPartition_alphaLogDocLength),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints, 
                                         Ni=apply(dtm, 1, sum)),
                               inits=starting.values, nChains, nAdapt)
    }
    if (alphaModel=="modelled" & !is.null(wordPartition) & betaPartition) {
        starting.values$psi <- matrix(rep(wordfish.ml$psi, max(wordPartition)), ncol=max(wordPartition))
        starting.values$raw.beta <- matrix(rep(wordfish.ml$beta, max(wordPartition)), ncol=max(wordPartition))
        starting.values <- starting.values[-which(names(starting.values)=="alpha")]
        X.alpha <- model.matrix(alphaFormula, data=alphaData)
        ## use coefs from regression of median across 56 counts on xs as inits for doc-level intercepts
        median.length <- round(apply(dtm, 1, median))
        startval.alpha <- glm.fit(X.alpha, median.length, family=poisson(link="log"))
        # add coefficients for alpha as starting values
        starting.values$coef.alpha <- as.numeric(coef(startval.alpha))
        jags.mod <- jags.model(textConnection(wfmodel_psibetaWordPartition_alphaModelled),
                               data=list(Y=dtm, wordPartition=wordPartition, dir=dir, wordConstraints=wordConstraints, 
                                         X.alpha=X.alpha),
                               inits=starting.values, nChains, nAdapt)
    }
    
    
    ## Update the model
    
    if (verbose) cat("Updating model\n")
    update(jags.mod, nUpdate)
    if (verbose) cat("Sampling from the posterior\n")

    ## Sample from the posterior
    if (alphaModel=="free") {
        jsparams <- c("beta", "tau.beta", "psi", "tau.psi", "theta", "alpha", "tau.alpha")
    } else if (alphaModel=="logdoclength") {
        jsparams <- c("beta", "tau.beta", "psi", "tau.psi", "theta") 
    } else {
        jsparams <- c("beta", "tau.beta", "psi", "tau.psi", "alpha", "coef.alpha", "tau.alpha", "theta")
    }
    jags.samples <- coda.samples(jags.mod, jsparams, n.iter=nSamples, thin=nThin)

    ## save the samples for testing
    save(jags.samples, file="jsPre.Rdata")
  
    ## reorder the words back to the original ordering, if words were partitioned
    if (!is.null(wordPartition)) {
        # put psi and beta samples back in original order
        # need to build an index for as many constraints are there are
        # create a new index to replace the values, equal in length to the new word-level parameters
        psi.original.index <- order(original.word.index)
        for (i in 1:(max(wordPartition)-1)) {
            psi.original.index <- c(psi.original.index, order(original.word.index) + length(words(dtm))*i)
        }
        jags.samples[[1]][,grep("^psi", colnames(jags.samples[[1]]))] <-
            jags.samples[[1]][,grep("^psi", colnames(jags.samples[[1]]))[psi.original.index]]
        # only reorder the first V (number of words) set for beta unless betaPartition is true,
        # in which case reorder all the parameters just as with psi
        if (betaPartition) {
            beta.original.index <- psi.original.index
        } else {
            beta.original.index <- order(original.word.index)
        }
        jags.samples[[1]][,grep("^beta", colnames(jags.samples[[1]]))] <-
            jags.samples[[1]][,grep("^beta", colnames(jags.samples[[1]]))[beta.original.index]]
        dtm <- dtm[, order(original.word.index)]
    }

    ## save the samples for testing
    jags.samples.Post <- jags.samples
    save(jags.samples.Post, file="jsPost.Rdata")
  
    s <- summary(jags.samples)
    retval <- list(dir=dir,
                   theta = s$statistics[grep("^theta", rownames(s$statistics)), "Mean"],
                   alpha = s$statistics[grep("^alpha", rownames(s$statistics)), "Mean"],
                   beta = s$statistics[grep("^beta", rownames(s$statistics)), "Mean"],
                   psi = s$statistics[grep("^psi", rownames(s$statistics)), "Mean"],
                   docs = docs(dtm), 
                   words = words(dtm),
                   tau.beta = s$statistics[grep("^tau.beta", rownames(s$statistics)), "Mean"],
                   tau.alpha = s$statistics[grep("^tau.alpha", rownames(s$statistics)), "Mean"],
                   tau.psi = s$statistics[grep("^tau.psi", rownames(s$statistics)), "Mean"], 
                   ll=NULL, data=dtm, call=thecall,
                   se.theta = s$statistics[grep("^theta", rownames(s$statistics)), "SD"],
                   mcmc.model=jags.mod,
                   wordPartition=wordPartition,
                   mcmc.samples=jags.samples,
                   wordPartition = wordPartition,
                   wordConstraints = wordConstraints,
                   time.elapsed=(proc.time() - start.time)[3])
    ## save for testing
    #save(retval, file="retval.Rdata")
  
    if (alphaModel=="logdoclength") retval$alpha <- log(apply(dtm, 1, sum))

    if (alphaModel=="modelled") {
        retval$alphaData <- alphaData
        retval$alpha.coeff <- s$statistics[grep("^coef.alpha", rownames(s$statistics)), "Mean"]
        retval$alpha.coeff.se <- s$statistics[grep("^coef.alpha", rownames(s$statistics)), "SD"]
        retval$sigma.alpha <- 1/sqrt(s$statistics[grep("^tau.alpha", rownames(s$statistics)), "Mean"])
    }

    class(retval) <- c("wordfish", "wordfishMCMC", class(retval))
    if (verbose) cat(paste("Total time elapsed:", round(retval$time.elapsed/60,2), "minutes.\n"))
    return(retval)
}


####
#### global definitions of JAGS code for each of the nine model permutations
####

wfmodel_noWordPartition_alphaFree <- "
    data {
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
    }
    model {
        # loop over documents
        for (i in 1:N) {
            # if remove the following line and change alphaMC[i] to alpha[i] is MUCH FASTER
            # but alpha wanders (not identified in the location)
            #alphaMC[i] <- alpha[i] - mean(alpha)
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j] + theta[i] * beta[j]
                Y[i,j] ~ dpois(rate[i,j])
            }
            alpha[i] ~ dnorm(mu.alpha, tau.alpha)
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # loop over words
        for (j in 1:V) {  
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            psi[j] ~ dnorm(0, tau.psi)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        mu.alpha ~ dnorm(0, .01)
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
    }"

wfmodel_noWordPartition_alphaLogDocLength <- "
    ## Note - requires the length of the document to be passed as additional data Ni
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
    }
    model {
        # loop over documents
        for (i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- log(Ni[i]) + psi[j] + theta[i] * beta[j]
                Y[i,j] ~ dpois(rate[i,j])
            }
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # loop over words
        for (j in 1:V) {  
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            psi[j] ~ dnorm(0, tau.psi)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        tau.beta ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
    }"

wfmodel_noWordPartition_alphaModelled <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        # total covariates for alpha
        dimensions.alphacovars <- dim(X.alpha)
        K <- dimensions.alphacovars[2]
    }
    model {
        # loop over documents
        for (i in 1:N) {
            # alphaMC[i] <- alpha[i] - mean(alpha)
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j] + theta[i] * beta[j]
                Y[i,j] ~ dpois(rate[i,j])
            }
            alpha[i] ~ dnorm(mu.alpha[i], tau.alpha)
            mu.alpha[i] <- X.alpha[i,] %*% coef.alpha[]
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # coefficients for alpha
        for(k in 1:K) {
            coef.alpha[k] ~ dnorm(0, .001)
        }
        
        # loop over words
        for (j in 1:V) {  
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            psi[j] ~ dnorm(0, tau.psi)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
    }"

wfmodel_psiWordPartition_alphaFree <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        C <- max(wordPartition)
        kConstraints <- length(wordConstraints)
    }
    model {
        # loop over documents
        for (i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j, wordPartition[i]] + theta[i] * beta[j]
                Y[i, j] ~ dpois(rate[i, j])
            }
            alpha[i] ~ dnorm(mu.alpha, tau.alpha)
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # loop over words where partition params are equal by constraint
        for (j in 1:kConstraints) {
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            psi[j, 1] ~ dnorm(psi.mean[j], tau.psi.mean)
            # set the rest of the categories for the constraint equal to the first
            for (k in 2:C) {
                psi[j, k] <- psi[j, 1]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        # loop over remaining words
        for (j in 2:V) {
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            # loop for countries to define psi
            for (c in 1:C) {
                psi[j, c] ~ dnorm(psi.mean[j], tau.psi.mean)
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        mu.alpha ~ dnorm(0, .01)
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
        tau.psi.mean ~ dgamma(.01, .01)
    }"

wfmodel_psibetaWordPartition_alphaFree <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        C <- max(wordPartition)
        kConstraints <- length(wordConstraints)
    }
    model {
        # loop over documents
        for(i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j, wordPartition[i]] + theta[i] * beta[j, wordPartition[i]]
                Y[i, j] ~ dpois(rate[i, j])
            }
            alpha[i] ~ dnorm(mu.alpha, tau.alpha)
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # loop over words where partition params are equal by constraint
        for (j in 1:kConstraints) {
            raw.beta[j, 1] ~ dnorm(beta.mean[j], tau.beta)
            beta.mean[j] ~ dnorm(0, tau.beta)
            beta[j, 1] <- direction.constraint * raw.beta[j, 1]
            psi[j, 1] ~ dnorm(psi.mean[j], tau.psi.mean)
            # set the rest of the categories for the constraint equal to the first
            for (k in 2:C) {
                psi[j, k] <- psi[j, 1]
                beta[j, k] <- beta[j, 1]
                raw.beta[j, k] <- raw.beta[j, 1]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        # loop over remaining words
        for (j in 2:V) {
            # loop for countries to define psi
            for (c in 1:C) {
                psi[j, c] ~ dnorm(psi.mean[j], tau.psi.mean)
                raw.beta[j, c] ~ dnorm(beta.mean[j], tau.beta)
                beta[j, c] <- direction.constraint * raw.beta[j, c]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
            beta.mean[j] ~ dnorm(0, tau.beta)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        mu.alpha ~ dnorm(0, .01)
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.beta.mean ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
        tau.psi.mean ~ dgamma(.01, .01)
    }"

wfmodel_psiWordPartition_alphaModelled <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        C <- max(wordPartition)
        kConstraints <- length(wordConstraints)
        # total covariates for alpha
        dimensions.alphacovars <- dim(X.alpha)
        K <- dimensions.alphacovars[2]
    }
    model {
        # loop over documents
        for(i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j, wordPartition[i]] + theta[i] * beta[j]
                Y[i, j] ~ dpois(rate[i, j])
            }
            alpha[i] ~ dnorm(mu.alpha[i], tau.alpha)
            mu.alpha[i] <- X.alpha[i,] %*% coef.alpha[]
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # coefficients for alpha
        for(k in 1:K) {
            coef.alpha[k] ~ dnorm(0, .001)
        }

        # loop over words where partition params are equal by constraint
        for (j in 1:kConstraints) {
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            psi[j, 1] ~ dnorm(psi.mean[j], tau.psi.mean)
            # set the rest of the categories for the constraint equal to the first
            for (k in 2:C) {
                psi[j, k] <- psi[j, 1]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        # loop over remaining words
        for (j in 2:V) {
            raw.beta[j] ~ dnorm(0, tau.beta)
            beta[j] <- direction.constraint * raw.beta[j]
            # loop for countries to define psi
            for (c in 1:C) {
                psi[j,c] ~ dnorm(psi.mean[j], tau.psi.mean)
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
        tau.psi.mean ~ dgamma(.01, .01)
    }"

wfmodel_psibetaWordPartition_alphaLogDocLength <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        C <- max(wordPartition)
        kConstraints <- length(wordConstraints)
    }
    model {
        # loop over documents
        for(i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- log(Ni[i]) + psi[j, wordPartition[i]] + theta[i] * beta[j, wordPartition[i]]
                Y[i, j] ~ dpois(rate[i, j])
            }
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # loop over words where partition params are equal by constraint
        for (j in 1:kConstraints) {
            raw.beta[j, 1] ~ dnorm(beta.mean[j], tau.beta)
            beta.mean[j] ~ dnorm(0, tau.beta)
            beta[j, 1] <- direction.constraint * raw.beta[j, 1]
            psi[j, 1] ~ dnorm(psi.mean[j], tau.psi.mean)
            # set the rest of the categories for the constraint equal to the first
            for (k in 2:C) {
                psi[j, k] <- psi[j, 1]
                beta[j, k] <- beta[j, 1]
                raw.beta[j, k] <- raw.beta[j, 1]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        # loop over remaining words
        for (j in 2:V) {
            # loop for countries to define psi
            for (c in 1:C) {
                psi[j, c] ~ dnorm(psi.mean[j], tau.psi.mean)
                raw.beta[j, c] ~ dnorm(beta.mean[j], tau.beta)
                beta[j, c] <- direction.constraint * raw.beta[j, c]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
            beta.mean[j] ~ dnorm(0, tau.beta)
        }
         
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        tau.beta ~ dgamma(.01, .01)
        tau.beta.mean ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
        tau.psi.mean ~ dgamma(.01, .01)
    }"


wfmodel_psibetaWordPartition_alphaModelled <- "
    data {
        # input must always be documents in rows, words in columns
        dimensions <- dim(Y)
        N <- dimensions[1]
        V <- dimensions[2]
        C <- max(wordPartition)
        kConstraints <- length(wordConstraints)
        # total covariates for alpha
        dimensions.alphacovars <- dim(X.alpha)
        K <- dimensions.alphacovars[2]
    }
    model {
        # loop over documents
        for(i in 1:N) {
            # loop over words
            for (j in 1:V) {
                log(rate[i,j]) <- alpha[i] + psi[j, wordPartition[i]] + theta[i] * beta[j, wordPartition[i]]
                Y[i, j] ~ dpois(rate[i, j])
            }
            alpha[i] ~ dnorm(mu.alpha[i], tau.alpha)
            mu.alpha[i] <- X.alpha[i,] %*% coef.alpha[]
            raw.theta[i] ~ dnorm(0, 1)
            theta[i] <- direction.constraint * raw.theta[i]
        }
        
        # coefficients for alpha
        for(k in 1:K) {
            coef.alpha[k] ~ dnorm(0, .001)
        }

        # loop over words where partition params are equal by constraint
        for (j in 1:kConstraints) {
            raw.beta[j, 1] ~ dnorm(beta.mean[j], tau.beta)
            beta.mean[j] ~ dnorm(0, tau.beta)
            beta[j, 1] <- direction.constraint * raw.beta[j, 1]
            psi[j, 1] ~ dnorm(psi.mean[j], tau.psi.mean)
            # set the rest of the categories for the constraint equal to the first
            for (k in 2:C) {
                psi[j, k] <- psi[j, 1]
                beta[j, k] <- beta[j, 1]
                raw.beta[j, k] <- raw.beta[j, 1]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
        }
        # loop over remaining words
        for (j in 2:V) {
            # loop for countries to define psi
            for (c in 1:C) {
                psi[j, c] ~ dnorm(psi.mean[j], tau.psi.mean)
                raw.beta[j, c] ~ dnorm(beta.mean[j], tau.beta)
                beta[j, c] <- direction.constraint * raw.beta[j, c]
            }
            psi.mean[j] ~ dnorm(0, tau.psi)
            beta.mean[j] ~ dnorm(0, tau.beta)
        }
        
        # stuff for identification, and direction constraints
        direction.constraint <- 2*(step(raw.theta[dir[2]]-raw.theta[dir[1]]))-1
        tau.alpha ~ dgamma(.01, .01)
        tau.beta ~ dgamma(.01, .01)
        tau.beta.mean ~ dgamma(.01, .01)
        tau.psi ~ dgamma(.01, .01)
        tau.psi.mean ~ dgamma(.01, .01)
    }"


