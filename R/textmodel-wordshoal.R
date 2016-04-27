#' @rdname textmodel_fitted-class 
#' @export
setClass("textmodel_wordshoal_fitted",
         slots = c(priors = "numeric", 
                   tol = "numeric",
                   dir = "numeric",
                   theta = "numeric",
                   beta = "numeric",
                   psi = "numeric",
                   alpha = "numeric",
                   phi = "numeric",
                   docs = "character",
                   features = "character",
                   sigma = "numeric",
                   ll = "numeric",
                   dispersion = "character",
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
#' positions using conditional maximum likelihood, and then the positions from a particular author 
#' are scaled across groups using a second-level linear factor model.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @param data the dfm on which the model will be fit
#' @param groups a factor giving the group for each document
#' @param authors a factor giving the author for each document
#' @param dir set global identification by specifying the indexes for a pair of 
#'   document sources (eg speakers) such that \eqn{\hat{\theta}_{dir[1]} < \hat{\theta}_{dir[2]}}.
#' @param tol tolerance for convergence.  A convergence 
#'   threshold for the log-posterior of the model.
#' @return An object of class textmodel_fitted_wordshoal.  This is a list 
#'   containing: \item{dir}{global identification of the dimension} 
#'   \item{theta}{estimated document positions} \item{alpha}{estimated document 
#'   fixed effects} \item{beta}{estimated feature marginal effects} 
#'   \item{psi}{estimated word fixed effects} \item{docs}{document labels} 
#'   \item{features}{feature labels} \item{sigma}{regularization parameter for 
#'   betas in Poisson form} \item{ll}{log likelihood at convergence} 
#'   \item{se.theta}{standard errors for theta-hats} \item{data}{dfm to which 
#'   the model was fit}
#' @details The returns match those of Will Lowe's R implementation of 
#'   \code{wordfish} (see the austin package), except that here we have renamed 
#'   \code{words} to be \code{features}.  (This return list may change.)  We 
#'   have also followed the practice begun with Slapin and Proksch's early 
#'   implementation of the model that used a regularization parameter of 
#'   se\eqn{(\sigma) = 3}, through the third element in \code{priors}.
#' @references Benjamin E Lauderdale and Alexander Herzog.  2016. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{Political Analysis}.
#' @author Benjamin Lauderdale and Kenneth Benoit
#' @examples
#' textmodel_wordshoal(ExampleData,groups,authors,dir = c(1,5))
#' 
#' @export
textmodel_wordshoal <- function(data, groups, authors, dir = c(1, 2), tol = 1e-3) {
    
    # check that no rows or columns are all zero
    zeroLengthDocs <- which(ntoken(data) == 0)
    if (length(zeroLengthDocs)) {
        data <- data[-zeroLengthDocs, ]
        cat("Note: removed the following zero-token documents:", docnames(data[zeroLengthDocs, ]), "\n")
    }
    zeroLengthFeatures <- which(docfreq(data) == 0)
    if (length(zeroLengthFeatures)) {
            data <- data[, -zeroLengthFeatures]
        cat("Note: removed the following zero-count features:", features(data[, zeroLengthFeatures]), "\n")
    }
    if (length(zeroLengthDocs) | length(zeroLengthFeatures)) cat("\n")

    # some error checking
    
    # MISSING check length(groups) = length(authors) = number of docs in data
    # MISSING check that groups and authors are factor variables
    
    if (length(tol) != 1)
        stop("tol requires 1 element")
    if (!is.numeric(priors) | !is.numeric(tol))
        stop("tol must be numeric")
    
    ## MISSING Loop extracting the dfms for each group, fitting wordfish, saving relevant bits of info
    
    
    wfresult <- wordfishcpp(as.matrix(data), as.integer(dir), 1/(priors^2), tol, disp, dispersionFloor)
    # NOTE: psi is a 1 x nfeature matrix, not a numeric vector
    #       alpha is a ndoc x 1 matrix, not a numeric vector
    
    
    
    ## MISSING Fit second-level model here
    
    
    new("textmodel_wordshoal_fitted", 
        x = data,
        docs = docnames(data), 
        features = features(data),
        dir = dir,
        dispersion = dispersion,
        priors = priors,
        theta = wfresult$theta,
        beta = wfresult$beta,
        psi = as.numeric(wfresult$psi),
        alpha = as.numeric(wfresult$alpha),
        phi = as.numeric(wfresult$phi),
        se.theta = wfresult$thetaSE ,
        call = match.call())
}

###########################################

#' @rdname textmodel_wordshoal
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print
#' @param ... additional arguments passed to \code{\link{print}}
#' @export
#' @method print textmodel_wordshoal_fitted
print.textmodel_wordshoal_fitted <- function(x, n=30L, ...) {
    cat("Fitted wordshoal model:\n")
    cat("Call:\n\t")
    print(x@call)
    cat("\nEstimated document positions:\n\n")
    results <- data.frame(Documents=docnames(x@x),
                          theta = x@theta,
                          SE = x@se.theta,
                          lower = x@theta - 1.96*x@se.theta,
                          upper = x@theta + 1.96*x@se.theta)
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

#' @rdname textmodel_wordshoal
#' @param object wordshoal fitted or predicted object to be shown
#' @export
setMethod("show", signature(object = "textmodel_wordshoal_fitted"), function(object) print(object))

#' @rdname textmodel_wordshoal
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
    
    rownames(results) <- object@authors
    print(results, ...)
    invisible(results)
}




