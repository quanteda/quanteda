
#' Wordfish text model
#' 
#' Estimate Slapin and Proksch's (2008) "wordfish" Poisson scaling model of 
#' one-dimensional document positions using conditional maximum likelihood.
#' @importFrom Rcpp evalCpp
#' @param x the dfm on which the model will be fit
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
#' @param dispersion sets whether a quasi-Poisson quasi-likelihood should be 
#'   used based on a single dispersion parameter (\code{"poisson"}), or 
#'   quasi-Poisson (\code{"quasipoisson"})
#' @param dispersion_level sets the unit level for the dispersion parameter, 
#'   options are \code{"feature"} for term-level variances, or \code{"overall"} 
#'   for a single dispersion parameter
#' @param dispersion_floor constraint for the minimal underdispersion multiplier 
#'   in the quasi-Poisson model.  Used to minimize the distorting effect of 
#'   terms with rare term or document frequencies that appear to be severely 
#'   underdispersed.  Default is 0, but this only applies if \code{dispersion = 
#'   "quasipoisson"}.
#' @param sparse specifies whether the \code{"dfm"} is coerced to dense
#' @param abs_err specifies how the convergence is considered
#' @param svd_sparse uses svd to initialize the starting values of theta, 
#'   only applies when \code{sparse = TRUE}
#' @param residual_floor specifies the threshold for residual matrix when 
#'   calculating the svds, only applies when \code{sparse = TRUE}
#' @return An object of class \code{textmodel_fitted_wordfish}.  This is a list 
#'   containing: \item{dir}{global identification of the dimension} 
#'   \item{theta}{estimated document positions} \item{alpha}{estimated document 
#'   fixed effects} \item{beta}{estimated feature marginal effects} 
#'   \item{psi}{estimated word fixed effects} \item{docs}{document labels} 
#'   \item{features}{feature labels} \item{sigma}{regularization parameter for 
#'   betas in Poisson form} \item{ll}{log likelihood at convergence} 
#'   \item{se.theta}{standard errors for theta-hats} \item{x}{dfm to which 
#'   the model was fit}
#' @details The returns match those of Will Lowe's R implementation of 
#'   \code{wordfish} (see the austin package), except that here we have renamed 
#'   \code{words} to be \code{features}.  (This return list may change.)  We 
#'   have also followed the practice begun with Slapin and Proksch's early 
#'   implementation of the model that used a regularization parameter of 
#'   se\eqn{(\sigma) = 3}, through the third element in \code{priors}.
#'   
#' @note In the rare situation where a warning message of "The algorithm did not
#'   converge." shows up, removing some documents may work.
#'   
#' @references Jonathan Slapin and Sven-Oliver Proksch.  2008. "A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts." \emph{American 
#'   Journal of Political Science} 52(3):705-772.
#'   
#'   Lowe, Will and Kenneth Benoit. 2013. "Validating Estimates of Latent Traits
#'   from Textual Data Using Human Judgment as a Benchmark." \emph{Political Analysis}
#'   21(3), 298-313. \url{http://doi.org/10.1093/pan/mpt002}
#' @author Benjamin Lauderdale, Haiyan Wang, and Kenneth Benoit
#' @examples
#' (wf <- textmodel_wordfish(data_dfm_lbgexample, dir = c(1,5)))
#' summary(wf)
#' coef(wf)
#' predict(wf)
#' predict(wf, se.fit = TRUE)
#' predict(wf, interval = 'confidence')
#' 
#' \dontrun{
#' ie2010dwf <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
#' (wf1 <- textmodel_wordfish(ie2010dfm, dir = c(6,5)))
#' (wf2a <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersion_floor = 0))
#' (wf2b <- textmodel_wordfish(ie2010dfm, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersion_floor = .5))
#' plot(wf2a$phi, wf2b$phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0))
#' plot(wf2a$phi, wf2b$phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0), type = "n")
#' underdispersedTerms <- sample(which(wf2a$phi < 1.0), 5)
#' which(featnames(ie2010dfm) %in% names(topfeatures(ie2010dfm, 20)))
#' text(wf2a$phi, wf2b$phi, wf2a$features, 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "grey90")
#' text(wf2a$phi['underdispersedTerms'], wf2b$phi['underdispersedTerms'], 
#'      wf2a$features['underdispersedTerms'], 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "black")
#' if (require(austin)) {
#'     wf_austin <- austin::wordfish(quanteda::as.wfm(ie2010dfm), dir = c(6,5))
#'     cor(wf1$theta, wf_austin$theta)
#' }}
#' @export
textmodel_wordfish <- function(x, dir = c(1, 2), 
                               priors = c(Inf, Inf, 3, 1), 
                               tol = c(1e-6, 1e-8), 
                               dispersion = c("poisson", "quasipoisson"), 
                               dispersion_level = c("feature", "overall"),
                               dispersion_floor = 0,
                               sparse = TRUE, 
                               abs_err = FALSE,
                               svd_sparse = TRUE,
                               residual_floor = 0.5) {
    UseMethod("textmodel_wordfish")
}
    
#' @export
textmodel_wordfish.default <- function(x, dir = c(1, 2), 
                                       priors = c(Inf, Inf, 3, 1), 
                                       tol = c(1e-6, 1e-8),
                                       dispersion = c("poisson", "quasipoisson"), 
                                       dispersion_level = c("feature", "overall"),
                                       dispersion_floor = 0,
                                       sparse = TRUE, 
                                       abs_err = FALSE,
                                       svd_sparse = TRUE,
                                       residual_floor = 0.5) {
    stop(friendly_class_undefined_message(class(x), "textmodel_wordfish"))
}

#' @export
textmodel_wordfish.dfm <- function(x, dir = c(1, 2), 
                                   priors = c(Inf, Inf, 3, 1), 
                                   tol = c(1e-6, 1e-8), 
                                   dispersion = c("poisson", "quasipoisson"), 
                                   dispersion_level = c("feature", "overall"),
                                   dispersion_floor = 0,
                                   sparse = TRUE, 
                                   abs_err = FALSE,
                                   svd_sparse = TRUE,
                                   residual_floor = 0.5) {
    
    x <- as.dfm(x)
    dispersion <- match.arg(dispersion)
    dispersion_level <- match.arg(dispersion_level)
    
    # check that no rows or columns are all zero
    empty_docs <- which(ntoken(x) == 0)
    if (length(empty_docs)) {
        catm("Note: removed the following zero-token documents:", 
             docnames(x)[empty_docs], "\n")
        x <- x[empty_docs * -1, ]
    }
    empty_feats <- which(docfreq(x) == 0)
    if (length(empty_feats)) {
        catm("Note: removed the following zero-count features:", 
             featnames(x)[empty_feats], "\n")
        x <- x[, empty_feats * -1]
    }
    if (length(empty_docs) || length(empty_feats)) catm("\n")
    
    # some error checking
    if (length(priors) != 4)
        stop("priors requires 4 elements")
    if (length(tol) != 2)
        stop("tol requires 2 elements")
    if (!is.numeric(priors) || !is.numeric(tol))
        stop("priors and tol must be numeric")
    if (dispersion_floor < 0 || dispersion_floor > 1.0)
        stop("dispersion_floor must be between 0 and 1.0")
    
    if (dispersion == "poisson" && dispersion_floor != 0)
        warning("dispersion_floor argument ignored for poisson")
    
    # check quasi-poisson settings and translate into numerical values  
    # 1 = Poisson, 
    # 2 = quasi-Poisson, overall dispersion, 
    # 3 = quasi-Poisson, term dispersion, 
    # 4 = quasi-Poisson, term dispersion w/floor
    if (dispersion == "poisson") {
        disp <- 1L
    } else if (dispersion == "quasipoisson" && dispersion_level == "overall") {
        disp <- 2L
    } else if (dispersion == "quasipoisson" && dispersion_level == "feature") {
        if (dispersion_floor) {
            disp <- 4L
        } else {
            disp <- 3L
        }
    } else {
        stop("Illegal option combination.")
    }
    if (sparse == TRUE) {
        result <- qatd_cpp_wordfish(x, as.integer(dir), 1 / (priors ^ 2), 
                                    tol, disp, 
                                    dispersion_floor, abs_err, svd_sparse, 
                                    residual_floor)
    } else{
        result <- qatd_cpp_wordfish_dense(as.matrix(x), 
                                          as.integer(dir), 1 / (priors ^ 2), 
                                          tol, disp, 
                                          dispersion_floor, abs_err)
    }
    # NOTE: psi is a 1 x nfeat matrix, not a numeric vector
    #       alpha is a ndoc x 1 matrix, not a numeric vector
    if (any(is.nan(result$theta))) 
        warning("Warning: The algorithm did not converge.")
    
    result <- list(
        x = x,
        docs = docnames(x),
        features = featnames(x),
        dir = dir,
        dispersion = dispersion,
        priors = priors,
        theta = as.numeric(result$theta),
        beta = as.numeric(result$beta),
        psi = as.numeric(result$psi),
        alpha = as.numeric(result$alpha),
        phi = as.numeric(result$phi),
        se.theta = as.numeric(result$thetaSE) ,
        call = match.call()
    )
    class(result) <- c('textmodel_wordfish', 'list')
    return(result)
}

#' @export
predict.textmodel_wordfish <- function(object, 
                                       newdata = NULL, 
                                       se.fit = FALSE,
                                       interval = c("none", "confidence"), level = 0.95,
                                       ...) {
    
    interval <- match.arg(interval)
    
    if (!is.null(newdata))
        stop('Prediction by newdata is not yet implimented\n')
    
    fit <- object$theta
    names(fit) <- object$docs
    fit_se <- object$se.theta
    
    result <- list(fit = fit)
    
    if (!se.fit && interval == 'none')
        return(result[[1]])
    
    if (interval == 'none') {
        result$se <- fit_se
        return(result)
    } else {
        z <- stats::qnorm(1 - (1 - level) / 2)
        result$lwr <- fit - z * fit_se
        result$upr <- fit + z * fit_se
        result <- as.data.frame(result, row.names = names(fit), check.names = FALSE)
        return(as.matrix(result))
    }
}

#' @rdname textmodel-internal
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print
#' @param ... additional arguments passed to \code{\link{print}}
#' @export
#' @method print textmodel_wordfish
print.textmodel_wordfish <- function(x, ...) {
    cat("Fitted wordfish model:\n")
    print(x$call)
    cat("\n")
}

#' @export
#' @method summary textmodel_wordfish
summary.textmodel_wordfish <- function(object, n = 30, ...) {
    
    stat <- data.frame(
        theta = object$theta,
        se = object$se.theta,
        row.names = object$docs,
        check.rows = FALSE,
        stringsAsFactors = FALSE
    )
    
    coef <- object$beta
    names(coef) <- object$features
    result <- list(
        'call' = object$call,
        'estimated.document.positions' = as.textmodel_statistics(stat),
        'estimated.feature.scores' = as.textmodel_coefficients(head(coef, n))
    )
    return(as.textmodel_summary(result))
}

#' @rdname textmodel-internal
#' @export
coef.textmodel_wordfish <- function(object, ...) {
    list(beta = object$beta,
         theta = object$theta,
         se.theta = object$se.theta,
         psi = object$psi,
         alpha = object$alpha)
}

#' @rdname textmodel-internal
#' @export
coefficient.textmodel_wordfish <- function(object, ...) {
    UseMethod('coef')   
}
