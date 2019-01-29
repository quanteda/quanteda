#' Class affinity maximum likelihood text scaling model
#'
#' \code{textmodel_affinity} implements the maximum likelihood supervised text
#' scaling method described in Perry and Benoit (2017).
#' @param x the \link{dfm} or \link{bootstrap_dfm} object on which the model
#'   will be fit.  Does not need to contain only the training documents, since
#'   the index of these will be matched automatically.
#' @param y vector of training classes/scores associated with each document
#'   identified in \code{data}
#' @param exclude a set of words to exclude from the model
#' @param smooth a smoothing parameter for class affinities; defaults to 0.5
#'   (Jeffreys prior). A plausible alternative would be 1.0 (Laplace prior).
#' @param ref_smooth a smoothing parameter for token distributions;
#'   defaults to 0.5
#' @param verbose logical; if \code{TRUE} print diagnostic information during
#'   fitting.
#' @author Patrick Perry and Kenneth Benoit
#' @references Perry, P.O. & Benoit, K.R. (2017). Scaling Text with
#'   the Class Affinity Model.
#'   \href{http://arxiv.org/abs/1710.08963}{arXiv:1710.08963 [stat.ML]}.
#' @examples
#' (af <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA)))
#' predict(af)
#' predict(af, newdata = data_dfm_lbgexample[6, ])
#'
#' \dontrun{
#' # compute bootstrapped SEs
#' dfmat <- bootstrap_dfm(data_corpus_dailnoconf1991, n = 10, remove_punct = TRUE)
#' textmodel_affinity(dfmat, y = c("Govt", "Opp", "Opp", rep(NA, 55)))
#' }
#' @export
#' @keywords textmodel experimental
#' @importFrom methods as
#' @importFrom stats sd predict
#' @seealso \code{\link{predict.textmodel_affinity}} for methods of applying a
#'   fitted \link{textmodel_affinity} model object to predict quantities from
#'   (other) documents.
textmodel_affinity <- function(x, y, exclude = NULL,
                               smooth = 0.5, ref_smooth = 0.5,
                               verbose = quanteda_options("verbose")) {
    UseMethod("textmodel_affinity")
}

#' @export
textmodel_affinity.default <- function(x, y, exclude = NULL,
                                       smooth = 0.5, ref_smooth = 0.5,
                                       verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "textmodel_affinity"))
}
    

#' @export
textmodel_affinity.dfm <- function(x, y, exclude = NULL,
                                   smooth = 0.5, ref_smooth = 0.5,
                                   verbose = quanteda_options("verbose")) {
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
        
    # estimate reference distributions
    counts <- dfm(x[!is.na(y),], groups = y[!is.na(y)], tolower = FALSE)

    # determine support set
    support <- colSums(counts) > 0

    # remove 'exclude' words from support
    if (!is.null(exclude)) {
        if (is.character(exclude)) {
            words <- colnames(x)
            support[words %in% exclude] <- FALSE
        } else {
            support[exclude] <- FALSE
        }
        counts[, !support] <- 0
        tots <- rowSums(counts)
    }

    # add Laplace-style smoothing, but only to words in the support
    counts[, support] <- counts[, support] + ref_smooth
    tots <- rowSums(counts)
    probs <- counts / tots

    p <- t(probs) # affinity expects transposed probabilities

    fitted <- affinity(p, t(x), smooth = smooth)
    result <- list(
        smooth = fitted$smooth,
        x = x,
        y = y,
        p = p,
        support = fitted$support,
        method = "affinity",
        call = match.call()
    )
    class(result) <- "textmodel_affinity"
    return(result)
}

#' @export
textmodel_affinity.dfm_bootstrap <- function(x, y, exclude = NULL,
                                             smooth = 0.5, ref_smooth = 0.5,
                                             verbose = quanteda_options("verbose")) {
    if (verbose)
        message("Bootstrapping textmodel_affinity for ", ndoc(x[[1]]), " documents:")

    # compute the model for the original corpus
    if (verbose)
        message("   ...computing model for dfm from original texts")
    fitted <- textmodel_affinity(x[[1]], y, exclude, smooth, ref_smooth)
    predicted <- predict(fitted)

    # compute the replicates, save coefficients to an array
    coeff_replicates <- array(NA, dim = c(dim(coef(predicted)), length(x)))
    if (verbose)
        message("   ...computing bootstrapped models and saving coefficients: ", appendLF = FALSE)
    for (i in seq_len(length(x))) {
        message(i, " ", appendLF = FALSE)
        temp <- textmodel_affinity(x[[i]], y, exclude, smooth, ref_smooth)
        coeff_replicates[,,i] <- coef(predict(temp))
    }
    message("")
    if (verbose)
        message("   ...replacing original SEs with bootstrapped SEs")
    # replace analytical coefficients with sd of replicates of coefficients
    predicted$se <- apply(coeff_replicates, c(1, 2), sd)
    if (verbose)
        message("   ...finished.")
    predicted
}

## ============= Internal computation methods ========

#' Internal function to fit the likelihood scaling mixture model.
#'
#' Ken recommends you use \code{\link{textmodel_affinity}} instead.
#' @param p word likelihoods within classes, estimated from training data
#' @param x term-document matrix for document(s) to be scaled
#' @param smooth a misnamed smoothing parameter, either a scalar or a vector
#'   equal in length to the number of documents
#' @author Patrick Perry
#' @return a list with stuff
#' @examples
#' p <- matrix(c(c(5/6, 0, 1/6), c(0, 4/5, 1/5)), nrow = 3,
#'             dimnames = list(c("A", "B", "C"), NULL))
#' theta <- c(.2, .8)
#' q <- drop(p %*% theta)
#' x <- 2 * q
#' (fit <- affinity(p, x))
#' @keywords internal textmodel
#' @export
affinity <- function(p, x, smooth = 0.5, verbose = FALSE) {
    p <- as.matrix(p)
    ncat <- nrow(p)   # number of features
    ndist <- ncol(p)  # k or number of classes
    nfit <- NCOL(x)   # number of documents to be predicted
    
    distnames <- colnames(p)  # class labels in the training set
    
    xdim <- dim(x)  # dim of the test set (nfeat x ndoc)
    stopifnot(NROW(x) == ncat)
    
    if (length(smooth) == 1) {
        smooth <- rep(smooth, ndist)
    }
    stopifnot(length(smooth) == ndist)
    stopifnot(all(smooth >= 0))
    
    if (!inherits(x, "CsparseMatrix")) {
        x <- as.matrix(x)
    }
    fitnames <- colnames(x)  # document names to be fitted
    
    support <- !apply(p == 0, 1, all) # ignore categories with no support
    p <- p[support, , drop = FALSE]
    x <- x[support, , drop = FALSE]
    
    # fit to each column of x
    fit <- vector("list", nfit)
    if (is.matrix(x)) {
        for (j in seq_len(nfit)) {
            if (verbose) {
                cat("Fitting column ", j, "\n", sep="")
            }
            fit[[j]] <- affinity1(p, x[,j], smooth, verbose)
        }
    } else {
        x <- as(x, "dgCMatrix")
        val <- x@x
        row_ind <- x@i + 1 # switch from 0- to 1-based indexing
        col_ptr <- x@p + 1 #
        
        for (j in seq_len(nfit)) {
            if (verbose) {
                cat("Fitting column ", j, "\n", sep="")
            }
            
            start <- col_ptr[[j]]
            end <- col_ptr[[j+1]]
            len <- end - start
            ix <- seq.int(start, length.out=len)
            
            xj <- val[ix]
            ij <- row_ind[ix]
            pj <- p[ij,,drop=FALSE]
            
            fit[[j]] <- affinity1(pj, xj, smooth, verbose)
        }
    }
    
    # simplify results
    coefficients <- matrix(NA, nfit, ndist, dimnames=list(fitnames, distnames))
    se <- matrix(NA, nfit, ndist, dimnames=list(fitnames, distnames))
    cov <- array(NA, c(ndist, ndist, nfit),
                 dimnames=list(distnames, distnames, fitnames))
    
    for (j in seq_len(nfit)) {
        fitj <- fit[[j]]
        coefficients[j,] <- fitj$theta
        se[j,] <- fitj$theta_se
        cov[,,j] <- fitj$cov
    }
    
    # drop dimension if input x was a vector - NOT ANY MORE -kb
    if (is.null(xdim) && nfit == 1) {
        coefficients <- coefficients[1, drop = FALSE]
        se <- se[1, drop = FALSE]
        cov <- cov[,,1, drop = FALSE]
    }
    
    list(coefficients = coefficients, 
         se = se, 
         cov = cov, 
         smooth = smooth,
         support = support)
}


#
# internal function to maximize the affinity likelihood
# author: Patrick Perry
#
affinity1 <- function(p, x, smooth, verbose = FALSE) {
    ncat <- nrow(p)
    ndist <- ncol(p)
    
    objective <- function(theta, bweight = 0, gradient = TRUE, hessian = TRUE) {
        q <- drop(p %*% theta)
        
        loglik <- sum(x * ifelse(x == 0, 0, log(q)))
        penalty <- -sum(smooth * log(theta))
        barrier <- -sum(log(theta))
        value <- (-loglik) + penalty + bweight * barrier
        
        res <- list(loglik = loglik, 
                    penalty = penalty, 
                    barrier = barrier,
                    bweight = bweight, 
                    value = value)
        
        if (gradient || hessian) {
            score <- drop(t(p) %*% ifelse(x == 0, 0, x / q))
            grad <- (-score) - ((smooth + bweight) / theta)
            
            res[["score"]] <- score
            res[["grad"]] <- grad
        }
        
        if (hessian) {
            imat_sqrt <- p * ifelse(x == 0, 0, sqrt(x) / q)
            imat <- t(imat_sqrt) %*% imat_sqrt
            hess <- imat + diag((smooth + bweight) / theta ^ 2, length(theta))
            
            res[["imat"]] <- imat
            res[["hess"]] <- hess
        }
        
        res
    }
    
    
    residuals <- function(theta, nu, bweight = 0) {
        obj <- objective(theta, bweight, hessian = FALSE)
        a <- rep(1, length(theta))
        
        g <- (obj$grad) + a * nu
        h <- sum(theta) - 1
        norm <- sqrt(sum(g^2) + h^2)
        
        list(dual = g, primal = h, norm = norm)
    }
    
    
    newton_step <- function(theta, nu, bweight = 0) {
        obj <- objective(theta, bweight)
        a <- rep(1, length(theta))
        
        g <- (obj$grad) + a * nu
        h <- sum(theta) - 1
        
        H <- (obj$hess)
        
        Hi_a <- solve(H, a)
        Hi_g <- solve(H, g)
        s <- drop(-(t(a) %*% Hi_a))
        w <- drop(t(a) %*% Hi_g - h) / s
        v <- -(Hi_a * w) - Hi_g
        
        list(primal = v, dual = w)
    }
    
    
    optimize <- function(theta, nu, bweight = 0, verbose = FALSE) {
        tol <- 1e-8
        alpha <- 0.01
        beta <- 0.5
        
        resid <- residuals(theta, nu, bweight)
        iter <- 0
        
        while (resid$norm > tol || max(abs(resid$primal)) > tol) {
            if (verbose) {
                cat("iteration: ", iter, "; residual norm: ", resid$norm, "\n",
                    sep="")
            }
            step <- newton_step(theta, nu, bweight)
            
            tmax <- min(ifelse(step$primal >= 0, Inf, -theta / step$primal))
            t <- min(1, tmax)
            
            repeat {
                theta1 <- theta + t * step$primal
                nu1 <- nu + t * step$dual
                
                if (all(theta1 > 0)) {
                    resid1 <- residuals(theta1, nu1, bweight)
                    if (resid1$norm <= (1 - alpha * t) * resid$norm) {
                        break
                    }
                }
                t <- beta * t
            }
            theta <- theta1
            nu <- nu1
            resid <- resid1
            iter <- iter + 1
        }
        
        obj <- objective(theta, bweight)
        list(theta = theta, 
             nu = nu, 
             resid = resid, 
             objective = obj, 
             iter = iter)
    }
    
    
    shrink_barrier <- function(theta, nu, bweight = 0, verbose = FALSE) {
        shrink <- 0.1
        
        if (verbose) {
            cat("bweight: ", bweight, "\n", sep="")
        }
        opt <- optimize(theta, nu, bweight, verbose = verbose)
        
        while (bweight >= 1e-8) {
            theta <- opt$theta
            nu <- opt$nu
            bweight <- shrink * bweight
            if (verbose) {
                cat("bweight: ", bweight, "\n", sep="")
            }
            opt <- optimize(theta, nu, bweight, verbose = verbose)
        }
        opt[["bweight"]] <- bweight
        opt
    }
    
    
    theta <- rep(1 / ndist, ndist)
    nu <- 0
    
    if (any(smooth == 0)) {
        bweight <- 100
        res <- shrink_barrier(theta, nu, bweight, verbose = verbose)
    } else {
        res <- optimize(theta, nu, 0, verbose = verbose)
    }
    
    obj <- res$objective
    
    # The covariance matrix C solves the block system
    #
    # [ H   a ] [ C   w ] = [ I 0 ]
    # [ a^T 0 ] [ w^T s ] = [ 0 1 ]
    #
    # where a = (1, 1, ..., 1)^T.
    #
    H <- obj$hess
    a <- rep(1, ndist)
    
    # First solve for w^t:
    #   w^T = (a^T H^{-1} a)^{-1} a^T H^{-1}
    #
    Hi <- solve(H)
    Hi_a <- solve(H, a)
    a_Hi_a <- sum(a * Hi_a)
    
    # Then substitute for C:
    #   C = H^{-1} - H^{-1} a (a^T H^{-1} a)^{-1} a^T H^{-1}
    #
    cov <- Hi - (1 / a_Hi_a) * (Hi_a %*% t(Hi_a))
    
    res[["cov"]] <- cov
    res[["theta_se"]] <- sqrt(pmax(diag(cov), 0))
    res
}

# supporting methods for textmodel_affinity ------------

#' @method print textmodel_affinity
#' @export
print.textmodel_affinity <- function(x, ...) {
    cat("Call:\n")
    print(x$call)
    
    ref <- table(x$y)
    namez <- names(ref)
    namez[2:length(namez)] <- paste(",", namez[2:length(namez)])
    
    cat("\n",
        "Training documents per class:", 
        paste0(interleave(paste0(namez, ": "), as.integer(ref)), collapse = ""), "; ",
        "total training features: ", nrow(x$p), "\n",
        sep = "")
}

## ============== Prediction Methods =======================================

#' Prediction for a fitted affinity textmodel
#'
#' @description
#' Estimate \eqn{\theta_i} for each document, from a fitted
#' \link{textmodel_affinity} object.
#' 
#' Other methods below provide standard ways to extract or compute quantities 
#' from predicted \link{textmodel_affinity} objects.
#' @param object a fitted affinity textmodel
#' @param level probability level for confidence interval width
#' @param newdata dfm on which prediction should be made
#' @param ... unused
#' @return \code{predict()} returns a list of predicted affinity textmodel
#'   quantities.
#' @importFrom methods new
#' @keywords textmodel internal
#' @seealso \code{\link{influence.predict.textmodel_affinity}} for methods of
#'   computing the influence of particular features from a predicted
#'   \link{textmodel_affinity} model.
#' @export
predict.textmodel_affinity <- function(object, newdata = NULL,
                                       level = 0.95, ...) {
    if (length(list(...)) > 0)
        stop("Arguments:", names(list(...)), "not supported.\n")
    
    if (!is.null(newdata)) {
        data <- newdata
        train <- rep(FALSE, nrow(data))
    } else {
        data <- object$x
        newdata <- data
        train <- !is.na(object$y)
    }
    
    predicted <- affinity(object$p, t(newdata), smooth = object$smooth)
    
    result <- list(
        coefficients = predicted$coefficients,
        se = predicted$se,
        cov = predicted$cov,
        smooth = object$smooth,
        newdata = newdata,
        train = train,
        level = level,
        p = object$p,
        support = object$support)
    class(result) <- c("predict.textmodel_affinity", "list")
    return(result)
}

#' @method print predict.textmodel_affinity
#' @export
print.predict.textmodel_affinity <- function(x, ...) {
    print(unclass(x))
}

#' @rdname predict.textmodel_affinity
#' @method coef predict.textmodel_affinity
#' @return \code{coef()} returns a document \eqn{\times} class matrix of class
#'   affinities for each document.
#' @export
coef.predict.textmodel_affinity <- function(object, ...) {
    object$coefficients
}

#' @method coefficients predict.textmodel_affinity
#' @export
coefficients.predict.textmodel_affinity <- function(object, ...) {
    UseMethod("coef")   
}

#' @rdname predict.textmodel_affinity
#' @return 
#' \code{residuals()} returns a document-by-feature matrix of residuals.
#' \code{resid()} is an alias.
#' @method residuals predict.textmodel_affinity
#' @param type see \link{residuals.lm}
#' @importFrom stats residuals resid
#' @export
residuals.predict.textmodel_affinity <- function(object, type = c("response", "pearson"), ...) {
          
    type <- match.arg(type)
    expected <- coef(object) %*% t(object$p) * rowSums(object$newdata)
    r <- object$newdata - expected
    if (type == "response") {
        res <- r
    } else if (type == "pearson") {
        res <- r / sqrt(expected)
    }
    res[,!object$support] <- NA
    as.matrix(res)
}

#' @export
#' @method resid predict.textmodel_affinity
resid.predict.textmodel_affinity <- function(object, ...) {
    UseMethod("residuals", ...)
}

#' @rdname predict.textmodel_affinity
#' @method rstandard predict.textmodel_affinity
#' @return \code{rstandard()} is a shortcut to return the pearson residuals.
#' @importFrom stats rstandard sd
#' @export
rstandard.predict.textmodel_affinity <- function(model, ...) {
    residuals(model, type = "pearson")
}


# ============== Influence methods =============

#' Compute feature influence from a predicted textmodel_affinity object
#'
#' Computes the influence of features on scaled \link{textmodel_affinity}
#' applications.
#' @param model a predicted
#'   \link[=predict.textmodel_affinity]{textmodel_affinity} object
#' @param subset whether to use all data or a subset (for instance, exclude the
#'   training set)
#' @param ... unused
#' @seealso \code{\link{influence.lm}}
#' @keywords textmodel internal
#' @importFrom stats influence
#' @method influence predict.textmodel_affinity
#' @import Matrix
#' @examples 
#' tmot <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
#' pred <- predict(tmot) 
#' influence(pred)
#' @export
influence.predict.textmodel_affinity <- function(model, subset = !train, ...) {
    # subset/training set
    train <- model$train
    
    # reference distributions
    p <- model$p
    levels <- colnames(p)
    support <- model$support
    
    # class affinity estimates
    theta <- model$coefficients[subset,,drop=FALSE]
    cov <- model$cov[,,subset,drop=FALSE]
    
    # data
    x <- model$newdata[subset,]
    x[,!support] <- 0
    x <- as(t(x), "dgCMatrix")
    nword <- nrow(x)
    words <- rownames(x)
    ntext <- ncol(x)
    texts <- colnames(x)
    
    val <- x@x
    row_ind <- x@i + 1 # switch from 0- to 1-based indexing
    col_ptr <- x@p + 1 #
    
    infl_norm <- numeric(length(val))
    infl_mode <- integer(length(val))
    rate <- numeric(length(val))
    
    for (j in seq_len(ntext)) {
        start <- col_ptr[[j]]
        end <- col_ptr[[j+1]]
        len <- end - start
        ix <- seq.int(start, length.out=len)
        
        xj <- val[ix]
        ij <- row_ind[ix]
        
        pj <- p[ij,,drop=FALSE]
        mu <- as.vector(pj %*% theta[j,])
        q <- pj / ifelse(mu == 0, 1, mu)
        
        infl_dir <- as.matrix(q %*% cov[,,j])
        h2 <- rowSums(q * infl_dir)
        
        # crude approximation for Hessian:
        # infl_norm[ix] <- 0.5 * xj * rowSums(abs(infl_dir))
        
        # more accurate approximation:
        infl_norm[ix] <- 0.5 * abs(xj / (1 - xj * h2)) * rowSums(abs(infl_dir))
        
        infl_mode[ix] <- apply(infl_dir, 1, which.max)
        rate[ix] <- xj / sum(xj)
    }
    
    # Note: why not just use Matrix::t()? KW
    transpose <- function(values, as_matrix = FALSE) {
        mat <- t(Matrix::sparseMatrix(i = x@i, p = x@p, x = values,
                                      dims = c(nword, ntext),
                                      dimnames = list(words, texts),
                                      index1 = FALSE))
        if (as_matrix) {
            ans <- mat
        } else {
            ans <- mat@x
        }
        ans
    }
    
    norm <- transpose(infl_norm, as_matrix = TRUE)
    rate <- transpose(rate)
    count <- transpose(val)
    mode <- transpose(infl_mode)
    
    res <- list(norm = norm, count = count, rate = rate,
                mode = mode, levels = levels, subset = subset,
                support = support)
    class(res) <- "influence.predict.textmodel_affinity"
    res
}

#' @title Internal methods for textmodel_affinity
#' @description Internal print and summary methods for derivative
#'   \link{textmodel_affinity} objects.
#' @name textmodel_affinity-internal
#' @keywords textmodel internal
#' @method print influence.predict.textmodel_affinity
#' @param n how many coefficients to print before truncating
#' @export
print.influence.predict.textmodel_affinity <- function(x, n = 30, ...) {
    ans <- summary(x, ...)
    print(ans, n)
}

#' @rdname textmodel_affinity-internal
#' @method summary influence.predict.textmodel_affinity
#' @importFrom stats median
#' @export
summary.influence.predict.textmodel_affinity <- function(object, ...) {
    norm <- object$norm
    ntext <- nrow(norm)
    nword <- ncol(norm)
    words <- colnames(norm)
    
    val <- norm@x
    row_ind <- norm@i + 1
    col_ptr <- norm@p + 1
    mode <- object$mode
    count <- object$count
    rate <- object$rate
    
    count_val <- numeric(nword)
    mean_val <- numeric(nword)
    med_val <- numeric(nword)
    sd_val <- numeric(nword)
    max_val <- numeric(nword)
    
    max_count <- numeric(nword)
    max_rate <- numeric(nword)
    med_rate <- numeric(nword)
    max_dir <- numeric(nword)
    
    for (j in seq_len(nword)) {
        start <- col_ptr[[j]]
        end <- col_ptr[[j+1]]
        len <- end - start
        
        if (len > 0) {
            ix <- seq.int(start, length.out=len)
            xj <- val[ix]
            ij <- row_ind[ix]
            
            m <- which.max(xj)
            
            count_val[j] <- len
            mean_val[j] <- mean(xj)
            med_val[j] <- median(xj)
            sd_val[j] <- ifelse(len > 1, sd(xj), 0)
            max_val[j] <- xj[m] # == val[ix[m]]
            
            max_count[j] <- count[ix[m]]
            max_rate[j] <- rate[ix[m]]
            med_rate[j] <- median(rate[ix])
            max_dir[j] <- mode[ix[m]]
        } else {
            count_val[j] <- 0
            mean_val[j] <- 0
            med_val[j] <- 0
            sd_val[j] <- 0
            max_val[j] <- 0
            
            max_count[j] <- 0
            max_rate[j] <- 0
            med_rate[j] <- 0
            max_dir[j] <- NA
        }
    }
    
    labels <- object$levels
    levels <- seq_along(labels)
    max_dir <- factor(max_dir, levels, labels)
    
    result <- list(word = words, count = count_val,
                   mean = mean_val, median = med_val,
                   sd = sd_val, max = max_val,
                   direction = max_dir,
                   rate = med_rate,
                   support = object$support)
    class(result) <- "summary.influence.predict.textmodel_affinity"
    result
}

#' @rdname textmodel_affinity-internal
#' @method print summary.influence.predict.textmodel_affinity
#' @param n how many coefficients to print before truncating
#' @export
print.summary.influence.predict.textmodel_affinity <- function(x, n = 30, ...) {
    ix <- sort(x$median, decreasing = TRUE, index.return = TRUE)$ix
    influence <- x$median[ix]
    
    d <- data.frame(word = x$word,
                    count = x$count,
                    median = x$median,
                    max = x$max,
                    direction = x$direction)
    d <- d[ix,]
    rownames(d) <- d$word
    d$word <- NULL
    
    if (!is.null(n) && !is.na(n)) {
        n <- min(n, nrow(d))
        d <- d[seq_len(n),,drop=FALSE]
        
        cat("Top ", n, " influential words:\n\n", sep = "")
    }
    print(d)
    
    invisible(x)
}


# ========= Internal functions =========

# compute chi^2 goodness of fit
gof_chi2 <- function(x) {
    UseMethod("gof_chi2")
}
gof_chi2.predict.textmodel_affinity <- function(x) {
    rowSums(rstandard(x)[,x$support,drop=FALSE]^2)
}

# function to interleave two vector objects
# Example:
# interleave(letters[1:3], 1:3)
# ## [1] "a" "1" "b" "2" "c" "3"
interleave <- function(v1, v2) {
    ord1 <- 2 * (seq_along(v1)) - 1
    ord2 <- 2 * (seq_along(v2))
    c(v1, v2)[order(c(ord1, ord2))]
}



