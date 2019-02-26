# main methods --------------

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
#' @param sparse specifies whether the \code{"dfm"} is coerced to dense.  While
#'   setting this to \code{TRUE} will make it possible to handle larger dfm
#'   objects (and make execution faster), it will generate slightly different
#'   results each time, because the sparse SVD routine has a stochastic element.
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
#' @seealso \code{\link{predict.textmodel_wordfish}}  
#' @references Slapin, J. & Proksch, S.O. (2008). 
#'   \href{https://doi.org/10.1111/j.1540-5907.2008.00338.x}{A Scaling Model 
#'   for Estimating Time-Series Party Positions from Texts}. \emph{American 
#'   Journal of Political Science}, 52(3), 705--772.
#'   
#'   Lowe, W. & Benoit, K.R. (2013). \href{http://doi.org/10.1093/pan/mpt002}{Validating 
#'   Estimates of Latent Traits from Textual Data Using Human Judgment as a Benchmark}.  
#'   \emph{Political Analysis}, 21(3), 298--313.
#' @author Benjamin Lauderdale, Haiyan Wang, and Kenneth Benoit
#' @examples
#' (tmod1 <- textmodel_wordfish(data_dfm_lbgexample, dir = c(1,5)))
#' summary(tmod1, n = 10)
#' coef(tmod1)
#' predict(tmod1)
#' predict(tmod1, se.fit = TRUE)
#' predict(tmod1, interval = "confidence")
#' 
#' \dontrun{
#' dfmat <- dfm(data_corpus_irishbudget2010)
#' (tmod2 <- textmodel_wordfish(dfmat, dir = c(6,5)))
#' (tmod3 <- textmodel_wordfish(dfmat, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersion_floor = 0))
#' (tmod4 <- textmodel_wordfish(dfmat, dir = c(6,5), 
#'                              dispersion = "quasipoisson", dispersion_floor = .5))
#' plot(tmod3$phi, tmod4$phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0))
#' plot(tmod3$phi, tmod4$phi, xlab = "Min underdispersion = 0", ylab = "Min underdispersion = .5",
#'      xlim = c(0, 1.0), ylim = c(0, 1.0), type = "n")
#' underdispersedTerms <- sample(which(tmod3$phi < 1.0), 5)
#' which(featnames(dfmat) %in% names(topfeatures(dfmat, 20)))
#' text(tmod3$phi, tmod4$phi, tmod3$features, 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "grey90")
#' text(tmod3$phi['underdispersedTerms'], tmod4$phi['underdispersedTerms'], 
#'      tmod3$features['underdispersedTerms'], 
#'      cex = .8, xlim = c(0, 1.0), ylim = c(0, 1.0), col = "black")
#' if (requireNamespace("austin")) {
#'     tmod5 <- austin::wordfish(quanteda::as.wfm(dfmat), dir = c(6,5))
#'     cor(tmod1$theta, tmod5$theta)
#' }}
#' @export
textmodel_wordfish <- function(x, dir = c(1, 2), 
                               priors = c(Inf, Inf, 3, 1), 
                               tol = c(1e-6, 1e-8), 
                               dispersion = c("poisson", "quasipoisson"), 
                               dispersion_level = c("feature", "overall"),
                               dispersion_floor = 0,
                               sparse = FALSE, 
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
                                       sparse = FALSE, 
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
                                   sparse = FALSE, 
                                   abs_err = FALSE,
                                   svd_sparse = TRUE,
                                   residual_floor = 0.5) {
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
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
    class(result) <- c("textmodel_wordfish", "textmodel", "list")
    result
}

#' Prediction from a textmodel_wordfish method
#'
#' \code{predict.textmodel_wordfish()} returns estimated document scores and
#' confidence intervals.  The method is provided for consistency with other
#' \code{textmodel_*()} methods, but does not currently allow prediction on
#' out-of-sample data.
#' @param object a fitted wordfish model
#' @inheritParams predict.textmodel_wordscores
#' @keywords textmodel internal
#' @export
predict.textmodel_wordfish <- function(object, 
                                       se.fit = FALSE,
                                       interval = c("none", "confidence"), level = 0.95,
                                       ...) {
    if (length(list(...)) > 0) stop("Arguments:", names(list(...)), "not supported.\n")
    interval <- match.arg(interval)
    
    fit <- object$theta
    names(fit) <- object$docs
    
    if (!se.fit && interval == "none") {
        class(fit) <- c("predict.textmodel_wordfish", "numeric")
        return(fit)
    }
    
    result <- list(fit = fit)
    if (se.fit) result$se.fit <- object$se.theta
    if (interval == "confidence") {
        result$fit <- matrix(result$fit, ncol = 3, nrow = length(result$fit),
                             dimnames = list(names(result$fit), c("fit", "lwr", "upr")))
        z <- stats::qnorm(1 - (1 - level) / 2)
        result$fit[, "lwr"] <- fit - z * object$se.theta
        result$fit[, "upr"] <- fit + z * object$se.theta
    }
    class(result) <- c("predict.textmodel_wordscores", class(result))
    result
}

#' print method for a wordfish model
#' @param x for print method, the object to be printed
#' @param ... unused
#' @method print textmodel_wordfish
#' @keywords internal textmodel
#' @export
print.textmodel_wordfish <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Dispersion: ", x$dispersion, "; ",
        "direction: ", x$dir[1], ' < ' , x$dir[2], "; ",
        ndoc(x), " documents; ",
        nfeat(x), " features.",
        "\n",
        sep = "")
}

#' summary method for textmodel_wordfish
#' @param object a \link{textmodel_wordfish} object
#' @param n maximum number of features to print in summary
#' @param ... unused
#' @export
#' @method summary textmodel_wordfish
#' @keywords internal textmodel
summary.textmodel_wordfish <- function(object, n = 30, ...) {
    
    stat <- data.frame(
        theta = object$theta,
        se = object$se.theta,
        row.names = object$docs,
        check.rows = FALSE,
        stringsAsFactors = FALSE
    )
    
    result <- list(
        'call' = object$call,
        'estimated.document.positions' = as.statistics_textmodel(stat),
        'estimated.feature.scores' = as.coefficients_textmodel(head(coef(object)$features, n))
    )
    return(as.summary.textmodel(result))
}

#' @rdname predict.textmodel_wordfish
#' @param margin which margin of parameter estimates to return: both (in a
#'   list), or just document or feature parameters
#' @method coef textmodel_wordfish
#' @return \code{coef.textmodel_wordfish()} returns a matrix of estimated
#'   parameters coefficients for the specified margin.
#' @export
coef.textmodel_wordfish <- function(object, margin = c("both", "documents", "features"), ...) {
    margin <- match.arg(margin)
    result <- list(
        documents = matrix(cbind(object$theta, object$alpha), ncol = 2,
                           dimnames = list(docnames(object), c("theta", "alpha"))),
        features = matrix(cbind(object$beta, object$psi), ncol = 2,
                          dimnames = list(featnames(object), c("beta", "psi")))
    )
    if (margin == "documents") {
        result[["documents"]] 
    } else if (margin == "features") {
        result[["features"]]
    } else result
}

#' @export
#' @rdname predict.textmodel_wordfish
coefficients.textmodel_wordfish <- function(object, ...) {
    UseMethod("coef")   
}

#' @export
#' @method print predict.textmodel_wordfish
print.predict.textmodel_wordfish <- function(x, ...) {
    print(unclass(x))
}
