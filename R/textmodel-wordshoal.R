#' @rdname textmodel_fitted-class 
#' @export
setClass("textmodel_wordshoal_fitted",
         slots = c(tol = "numeric",
                   dir = "numeric",
                   theta = "numeric",
                   beta = "numeric",
                   alpha = "numeric",
                   psi = "numeric",
                   groups = "factor",
                   authors = "factor",
                   ll = "numeric",
                   se.theta = "numeric"),
         contains = "textmodel_fitted")

#' @rdname textmodel_fitted-class
#' @export
setClass("textmodel_wordshoal_predicted",
         slots = c(newdata = "dfm", level = "numeric",
                   predvals = "data.frame"),
         prototype = list(level = 0.95),
         contains = "textmodel_wordfish_fitted")


#' wordshoal text model
#' 
#' Estimate Lauderdale and Herzog's (2016) model for one-dimensional document author (eg speakers)
#' positions based on multiple groups of texts (eg debates).  Each group of texts is scaled using
#' Slapin and Proksch's (2008) "wordfish" Poisson scaling model of one-dimensional document
#' positions, and then the positions from a particular author are scaled across groups
#' using a second-level linear factor model, using conditional maximum likelihood.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @param x the \link{dfm} from which the model will be fit
#' @param groups the name of a variable in the document variables for data giving the 
#' document group for each document
#' @param authors the name of a variable in the document variables for data giving the 
#' author of each document
#' @param dir set global identification by specifying the indexes for a pair of 
#'   authors such that \eqn{\hat{\theta}_{dir[1]} < \hat{\theta}_{dir[2]}}.
#' @param tol tolerance for convergence.  A convergence 
#'   threshold for the log-posterior of the model.
#' @return An object of class textmodel_fitted_wordshoal.  This is a list 
#'   containing: \item{tol}{log-posterior tolerance used in fitting} 
#'   \item{dir}{global identification of the dimension} 
#'   \item{theta}{estimated document positions} \item{beta}{debate marginal effects} 
#'   \item{alpha}{estimated document fixed effects} 
#'   \item{psi}{estimated document debate-level positions} \item{groups}{document groups} 
#'   \item{authors}{document authors} \item{ll}{log likelihood at convergence} 
#'   \item{se.theta}{standard errors for theta-hats} \item{data}{corpus to which 
#'   the model was fit}
#' @details Returns estimates of relative author positions across the full corpus of texts.
#' @references Benjamin E Lauderdale and Alexander Herzog.  2016. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{Political Analysis}.
#' @author Benjamin Lauderdale and Kenneth Benoit
#' @keywords textmodel experimental
#' @examples
#' \dontrun{
#' data(ie30corpus, package = "quantedaData")
#' iedfm <- dfm(ie30corpus, removePunct = TRUE)
#' wordshoalfit <- 
#'     textmodel_wordshoal(iedfm, dir = c(7,1),
#'                         groups = docvars(ie30corpus, "debateID"), 
#'                         authors = docvars(ie30corpus, "member.name"))
#' fitdf <- merge(as.data.frame(summary(wordshoalfit)),
#'                docvars(ie30corpus), 
#'                by.x="row.names", by.y="member.name")
#' fitdf <- subset(fitdf,!duplicated(memberID))
#' aggregate(theta ~ party.name, data = fitdf, mean)
#' }
#' @importFrom stats dgamma dnorm
#' @export
textmodel_wordshoal <- function(x, groups, authors, dir = c(1,2), tol = 1e-3) {
    UseMethod("textmodel_wordshoal")
}

#' @noRd
#' @export
textmodel_wordshoal.dfm <- function(x, groups, authors, dir = c(1,2), tol = 1e-3) {
    
    startTime <- proc.time()
    
    groups <- as.factor(groups)
    authors <- as.factor(authors)
    
    S <- ndoc(x)
    psi <- rep(NA, S)
    
    N <- nlevels(authors)
    M <- nlevels(groups)
    
    ## FIRST-LEVEL SCALING ##
    
    cat("\nScaling ", M, " document groups", sep="")
    
    for (j in 1:M) {
        
        # Extract dfm rows for current document group
        groupdfm <- x[groups == levels(groups)[j], ]
        
        # Remove features that do not appear in XXat leastXX MORE THAN one document
        groupdfm <- groupdfm[, colSums(groupdfm) > 1]
        
        # Run wordfish on document group
        # wfresult <- wordfishcpp(as.matrix(groupdfm), c(1, 2), c(0, 0, 1/9, 1), c(1e-2, 1e-4), 1L, 0L)
        wfresult <- textmodel_wordfish(groupdfm, dir = dir, tol = c(tol, 1e-8))
        
        # Save the results
        # psi[groups == levels(groups)[j]] <- wfresult$theta
        psi[groups == levels(groups)[j]] <- wfresult@theta
        
        if (j %% 20 == 0) cat(j, sep="") else cat(".")
        
    }
    
    ## SECOND-LEVEL SCALING ##
    
    cat("\nFactor Analysis on Debate-Level Scales")	
    
    psi <- replace(psi,is.na(psi),0) # debates that failed to scale
    jVec <- as.integer(groups)
    iVec <- as.integer(authors)
    
    ## Factor Analysis on Debate Score Matrix ##
    
    prioralpha <- 0.5
    priorbeta <- 0.5
    priortheta <- 1
    priortau <- 1
    
    # Dumb (but deterministic!) initial values
    
    alpha <- rep(0,M)
    beta <- rep(0,M)
    theta <- seq(-2,2,length.out=N)
    tau <- rep(1,N)
    
    # Calculate initial log-posterior...
    
    lastlp <- -Inf
    lp <- sum(dnorm(alpha,0,prioralpha,log=TRUE))
    lp <- lp + sum(dnorm(beta,0,priorbeta,log=TRUE))
    lp <- lp + sum(dnorm(theta,0,priortheta,log=TRUE))
    lp <- lp + sum(dgamma(tau,1,1,log=TRUE))
    for (s in 1:S){
        lps = alpha[jVec[s]] + beta[jVec[s]]*theta[iVec[s]];
        lp = lp + dnorm(psi[s],lps,(tau[iVec[s]])^(-1/2),log=TRUE);
    }
    
    # Until log-posterior stops changing...
    
    while((lp - lastlp) > abs(tol)){	
        
        cat(".")
        
        # Update debate parameters
        
        priordebate <- solve(matrix(c(prioralpha^2,0,0,priorbeta^2),2,2)) 
        
        for (j in 1:M){
            locs <- which(jVec == j)
            Ytmp <- psi[locs]
            Xtmp <- cbind(1,theta[iVec[locs]])
            Wtmp <- diag(tau[iVec[locs]])
            coeftmp <- solve(t(Xtmp) %*% Wtmp %*% Xtmp + priordebate) %*% t(Xtmp) %*% Wtmp %*% Ytmp
            alpha[j] <- coeftmp[1]
            beta[j] <- coeftmp[2] 
        }
        
        # Update speaker parameters
        
        for (i in 1:N){
            locs <- which(iVec == i)
            Ytmp <- matrix(psi[locs] - alpha[jVec[locs]],ncol=1)
            Xtmp <- matrix(beta[jVec[locs]],ncol=1)
            coeftmp <- solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) %*% t(Xtmp) %*% Ytmp
            theta[i] <- coeftmp[1,1]
            mutmp <- solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) %*% t(Xtmp) %*% Xtmp %*% coeftmp
            tau[i] <- (priortau + 0.5*length(Ytmp))/(priortau+0.5*(sum(Ytmp^2) - mutmp*(priortheta^(-2))*mutmp))
        }
        
        # Recalculate log-posterior
        
        lastlp <- lp
        lp <- sum(dnorm(alpha,0,prioralpha,log=TRUE))
        lp <- lp + sum(dnorm(beta,0,priorbeta,log=TRUE))
        lp <- lp + sum(dnorm(theta,0,priortheta,log=TRUE))
        lp <- lp + sum(dgamma(tau,priortau,priortau,log=TRUE))
        for (s in 1:S){
            lps = alpha[jVec[s]] + beta[jVec[s]]*theta[iVec[s]];
            lp = lp + dnorm(psi[s],lps,(tau[iVec[s]])^(-1/2),log=TRUE);
        }  
        
    } # end while  
    
    ## Calculate standard errors for thetas
    
    thetaSE <- rep(NA,N)
    for (i in 1:N){
        locs <- which(iVec == i)
        Xtmp <- matrix(beta[jVec[locs]],ncol=1)
        thetaSE[i] <- sqrt(solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) / tau[i])
    }
    
    ## Return results 
    
    cat("\nElapsed time:", (proc.time() - startTime)[3], "seconds.\n")
    
    new("textmodel_wordshoal_fitted", 
        tol = tol,
        authors = authors,
        groups = groups,
        theta = theta,
        beta = beta,
        alpha = alpha,
        psi = psi,
        se.theta = thetaSE,
        call = match.call())
}

###########################################

#' @rdname textmodel-internal
#' @param x for print method, the object to be printed
#' @param ... additional arguments passed to \code{\link{print}}
#' @export
#' @method print textmodel_wordshoal_fitted
print.textmodel_wordshoal_fitted <- function(x, ...) {
    cat("Fitted wordshoal model:\n")
    cat("Call:\n\t")
    print(x@call)
    cat("\nEstimated author positions:\n\n")
    results <- data.frame(theta = x@theta,
                          SE = x@se.theta,
                          lower = x@theta - 1.96*x@se.theta,
                          upper = x@theta + 1.96*x@se.theta)
    rownames(results) <- levels(x@authors)
    print(results,...)
}

#' @rdname textmodel-internal
#' @keywords internal
#' @export
setMethod("show", signature(object = "textmodel_wordshoal_fitted"), function(object) print(object))

#' @rdname textmodel-internal
#' @keywords internal
#' @export
setMethod("show", signature(object = "textmodel_wordshoal_predicted"), function(object) print(object))


#' @export
#' @method summary textmodel_wordshoal_fitted
summary.textmodel_wordshoal_fitted <- function(object, ...) {
    cat("Call:\n\t")
    print(object@call)
    
    cat("\nEstimated document positions:\n")
    results <- data.frame(theta = object@theta,
                          SE = object@se.theta,
                          lower = object@theta - 1.96*object@se.theta,
                          upper = object@theta + 1.96*object@se.theta)
    
    rownames(results) <- levels(object@authors)
    print(results, ...)
    invisible(results)
}




