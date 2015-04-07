#' @rdname textmodel_fitted-class 
#' @export
setClass("textmodel_wordfish_fitted",
         slots = c(priors = "numeric", 
                   tol = "numeric",
                   dir = "numeric",
                   theta = "numeric",
                   beta = "numeric",
                   psi = "numeric",
                   alpha = "numeric",
                   docs = "character",
                   features = "character",
                   sigma = "numeric",
                   ll = "numeric",
                   se.theta = "numeric"),
         contains = "textmodel_fitted")

#' @rdname textmodel_fitted-class
#' @export
setClass("textmodel_wordfish_predicted",
         slots = c(newdata = "dfm", level = "numeric",
                   predvals = "data.frame"),
         prototype = list(level = 0.95),
         contains = "textmodel_wordfish_fitted")


#' wordfish text model
#' 
#' Estimate Slapin and Proksch's (2008) "wordfish" Poisson scaling model of 
#' one-dimensional document positions using conditional maximum likelihood.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @param data the dfm on which the model will be fit
#' @param dir set global identification by specifying the indexes for a pair of
#'   documents such that \eqn{\hat{\theta}_{dir[1]} < \hat{\theta}_{dir[2]}}.
#' @param priors priors for \eqn{\theta_i}, \eqn{\alpha_i}, \eqn{\psi_j}, and 
#'   \eqn{\beta_j} where \eqn{i} indexes documents and \eqn{j} indexes features
#' @param tol tolerances for convergence (explain why a pair)
#' @param ... additional arguments passed to other functions
#' @return An object of class textmodel_fitted_wordfish.  This is a list 
#'   containing: \item{dir}{global identification of the dimension} 
#'   \item{theta}{estimated document positions} \item{alpha}{estimated document 
#'   fixed effects} \item{beta}{estimated feature marginal effects} 
#'   \item{psi}{estimated word fixed effects} \item{docs}{document labels} 
#'   \item{features}{feature labels} \item{sigma}{regularization parameter for 
#'   betas in Poisson form} \item{ll}{log likelihood at convergence} 
#'   \item{se.theta}{standard errors for theta-hats} \item{data}{dfm to which 
#'   the model was fit}
#' @details The returns match those of Will Lowe's R implementation of
#'   \code{wordfish} (see the austin package), except that here we have renamed \code{words} to
#'   be \code{features}.  (This return list may change.)  We have also followed the practice begun with
#'   Slapin and Proksch's early implementation of the model that used a regularization parameter of 
#'   se\eqn{(\sigma) = 3}, through the third element in \code{priors}.
#' @references Jonathan Slapin and Sven-Oliver Proksch.  2008. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{American 
#'   Journal of Political Science} 52(3):705-772.
#' @author Benjamin Lauderdale and Kenneth Benoit
#' @examples
#' ie2010dfm <- dfm(ie2010Corpus, verbose=FALSE)
#' wfmodel <- textmodel_wordfish(LBGexample, dir = c(6,5))
#' wfmodel
#' 
#' \dontrun{if (require(austin)) {
#'         wfmodelAustin <- wordfish(quanteda::as.wfm(LBGexample), dir = c(6,5))
#'         cor(wfmodel@@theta, wfmodelAustin$theta)
#' }}
#' @export
textmodel_wordfish <- function(data, dir = c(1, 2), priors = c(Inf, Inf, 3, 1), tol = c(1e-6, 1e-8)) {
    wfresult <- wordfishcpp(as.matrix(data), as.integer(dir), 1/(priors^2), tol)
    # NOTE: psi is a 1 x nfeature matrix, not a numeric vector
    #       alpha is a ndoc x 1 matrix, not a numeric vector
    new("textmodel_wordfish_fitted", 
        x = data,
        docs = docnames(data), 
        features = features(data),
        dir = dir,
        priors = priors,
        theta = wfresult$theta,
        beta = wfresult$beta,
        psi = as.numeric(wfresult$psi),
        alpha = as.numeric(wfresult$alpha),
        call = match.call())
}

###########################################

#' @rdname textmodel_wordfish
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print 
#' @export
#' @method print textmodel_wordfish_fitted
print.textmodel_wordfish_fitted <- function(x, n=30L, ...) {
    cat("Fitted wordfish model:\n")
    cat("Call:\n\t")
    print(x@call)
    cat("\nEstimated document positions:\n\n")
    results <- data.frame(Documents=docnames(x@x),
                          theta = x@theta,
                          SE = NA,
                          lower = NA,
                          upper = NA)
    print(results, ...)
    if (n>0) {
        cat("\nEstimated feature scores: ")
        if (length(x@psi) > n)
            cat("showing first", n, "beta-hats for features")
        cat("\n\n")
        tempBetas <- x@beta
        names(tempBetas) <- x@features
        print(head(tempBetas, n), ...)
    }
}

#' @rdname textmodel_wordfish
#' @param object wordfish fitted or predicted object to be shown
#' @export
setMethod("show", signature(object = "textmodel_wordfish_fitted"), function(object) print(object))

#' @rdname textmodel_wordfish
#' @export
setMethod("show", signature(object = "textmodel_wordfish_predicted"), function(object) print(object))


#' @export
#' @method summary textmodel_wordfish_fitted
summary.textmodel_wordfish_fitted <- function(object, ...) {
    cat("Call:\n\t")
    print(object@call)
    
    cat("\nEstimated document positions:\n")
    results <- data.frame(theta = object@theta,
                          SE = NA,
                          lower = NA,
                          upper = NA)
    
    rownames(results) <- object@docs
    print(results, ...)
    invisible(results)
}


# @rdname textmodel_wordfish
# @export
# @method print textmodel_wordfish_predicted
# print.textmodel_wordfish_predicted <- function(x, ...) {
#     cat("Predicted textmodel of type: wordfish\n\n")
#     #cat("Call:\n\t")
#     #print(object$call)
#     
#     x <- x@textfish
#     
#     ## options if a wordscores object 
#     ## (and no, it's not very object-oriented!)
#     names(x)[which(names(x)=="textscore_raw")] <- "textscore"
#     names(x)[which(names(x)=="textscore_raw_se")] <- "LBG se"
#     names(x)[which(names(x)=="textscore_raw_lo")] <- "ci lo"
#     names(x)[which(names(x)=="textscore_raw_hi")] <- "ci hi"
#     names(x)[which(names(x)=="textscore_mv")] <- "MV rescaled"
#     names(x)[which(names(x)=="textscore_lbg")] <- "LBG rescaled"
#     names(x)[which(names(x)=="textscore_lbg_lo")] <- "LBG lo"
#     names(x)[which(names(x)=="textscore_lbg_hi")] <- "LBG hi"
#     # pare down the output if rescaling has been specified
#     if (any(c("LBG rescaled", "MV rescaled") %in% names(x)))
#         x$ci.lo <- x$ci.hi <- NULL
#     
#     print(round(as.data.frame(x), 4))
#     cat("\n")
# }





