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
#' @param priors prior precisions for the estimated parameters \eqn{\alpha_i}, 
#'   \eqn{\psi_j}, \eqn{\beta_j}, and \eqn{\theta_i}, where \eqn{i} indexes 
#'   documents and \eqn{j} indexes features
#' @param tol tolerances for convergence.  The first value is a convergence 
#'   threshold for the log-posterior of the model, the second value is the 
#'   tolerance in the difference in parameter values from the iterative 
#'   conditional maximum likelihood (from conditionally estimating 
#'   document-level, then feature-level parameters).
#' @param dispersion sets whether a quasi-poisson quasi-likelihood should be 
#'   used based on a single dispersion parameter (\code{"poisson"}), or 
#'   quasi-Poisson (\code{"quasipoisson"})
#' @param dispersionLevel sets the unit level for the dispersion parameter, 
#'   options are \code{"feature"} for term-level variances, or \code{"overall"} 
#'   for a single dispersion parameter
#' @param dispersionFloor constraint for the minimal underdispersion multiplier 
#'   in the quasi-Poisson model.  Used to minimize the distorting effect of 
#'   terms with rare term or document frequencies that appear to be severely 
#'   underdispersed.  Default is 0, but this only applies if \code{dispersion = 
#'   "quasipoisson"}.
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
#'   \code{wordfish} (see the austin package), except that here we have renamed 
#'   \code{words} to be \code{features}.  (This return list may change.)  We 
#'   have also followed the practice begun with Slapin and Proksch's early 
#'   implementation of the model that used a regularization parameter of 
#'   se\eqn{(\sigma) = 3}, through the third element in \code{priors}.
#' @references Jonathan Slapin and Sven-Oliver Proksch.  2008. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{American 
#'   Journal of Political Science} 52(3):705-772.
#'   
#'   Lowe, Will and Kenneth Benoit. 2013. "Validating Estimates of Latent Traits
#'   from Textual Data Using Human Judgment as a Benchmark." \emph{Political Analysis}
#'   21(3), 298-313. \url{http://doi.org/10.1093/pan/mpt002}
#' @author Benjamin Lauderdale and Kenneth Benoit
#' @examples
#' textmodel_wordfish(LBGexample, dir = c(1,5))
#' 
#' \dontrun{
#' ie2010dfm <- dfm(ie2010Corpus, verbose = FALSE)
#' (wfm1 <- textmodel_wordfish(ie2010dfm, dir = c(6,5)))
#' (wfm2a <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersionFloor = 0))
#' (wfm2b <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersionFloor = .5))
#' plot(wfm2a@phi, wfm2b@phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0))
#' plot(wfm2a@phi, wfm2b@phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0), type = "n")
#' underdispersedTerms <- sample(which(wfm2a@phi < 1.0), 5)
#' which(features(ie2010dfm) %in% names(topfeatures(ie2010dfm, 20)))
#' text(wfm2a@phi, wfm2b@phi, wfm2a@features, 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "grey90")
#' text(wfm2a@phi[underdispersedTerms], wfm2b@phi[underdispersedTerms], 
#'      wfm2a@features[underdispersedTerms], 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "black")
#' if (require(austin)) {
#'     wfmodelAustin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
#'     cor(wfm1@@theta, wfm1Austin$theta)
#' }}
#' @export
textmodel_wordfish <- function(data, dir = c(1, 2), priors = c(Inf, Inf, 3, 1), tol = c(1e-6, 1e-8), 
                               dispersion = c("poisson", "quasipoisson"), 
                               dispersionLevel = c("feature", "overall"),
                               dispersionFloor = 0) {
    
    dispersion <- match.arg(dispersion)
    dispersionLevel <- match.arg(dispersionLevel)
    
    # check that no rows or columns are all zero
    zeroLengthDocs <- which(ntoken(data) == 0)
    if (length(zeroLengthDocs)) {
        data <- data[-zeroLengthDocs, ]
        catm("Note: removed the following zero-token documents:", docnames(data[zeroLengthDocs, ]), "\n")
    }
    zeroLengthFeatures <- which(docfreq(data) == 0)
    if (length(zeroLengthFeatures)) {
            data <- data[, -zeroLengthFeatures]
        catm("Note: removed the following zero-count features:", features(data[, zeroLengthFeatures]), "\n")
    }
    if (length(zeroLengthDocs) | length(zeroLengthFeatures)) catm("\n")

    # some error checking
    if (length(priors) != 4)
        stop("priors requires 4 elements")
    if (length(tol) != 2)
        stop("tol requires 2 elements")
    if (!is.numeric(priors) | !is.numeric(tol))
        stop("priors and tol must be numeric")
    if (dispersionFloor < 0 | dispersionFloor > 1.0)
        stop("dispersionFloor must be between 0 and 1.0")
    
    if (dispersion == "poisson" & dispersionFloor != 0)
        warning("dispersionFloor argument ignored for poisson")
    
#     if (length(addedArgs <- list(...)))
#         warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")

    # check quasi-poisson settings and translate into numerical values  
    # 1 = Poisson, 2 = quasi-Poisson, overall dispersion, 
    # 3 = quasi-Poisson, term dispersion, 4 = quasi-Poisson, term dispersion w/floor
    if (dispersion == "poisson") disp <- 1L
    else if (dispersion == "quasipoisson" & dispersionLevel == "overall") disp <- 2L
    else if (dispersion == "quasipoisson" & dispersionLevel == "feature") {
        if (dispersionFloor) disp <- 4L
        else disp <- 3L
    } else
        stop("Illegal option combination.")

    # catm("disp = ", disp, "\n")
    
    wfresult <- wordfishcpp(as.matrix(data), as.integer(dir), 1/(priors^2), tol, disp, dispersionFloor)
    # NOTE: psi is a 1 x nfeature matrix, not a numeric vector
    #       alpha is a ndoc x 1 matrix, not a numeric vector
    new("textmodel_wordfish_fitted", 
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

#' @rdname textmodel_wordfish
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print
#' @param ... additional arguments passed to \code{\link{print}}
#' @export
#' @method print textmodel_wordfish_fitted
print.textmodel_wordfish_fitted <- function(x, n=30L, ...) {
    cat("Fitted wordfish model:\n")
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
                          SE = object@se.theta,
                          lower = object@theta - 1.96*object@se.theta,
                          upper = object@theta + 1.96*object@se.theta)
    
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





