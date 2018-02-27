# main methods --------------

#' Wordscores text model
#' 
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003)
#' "Wordscores" method for scaling texts on a single dimension, given a set of
#' anchoring or \emph{reference} texts whose values are set through reference
#' scores. This scale can be fitted in the linear space (as per LBG 2003) or in
#' the logit space (as per Beauchamp 2012).  Estimates of \emph{virgin} or
#' unknown texts are obtained using the \code{predict()} method to score
#' documents from a fitted \code{textmodel_wordscores} object.
#' 
#' @param x the \link{dfm} on which the model will be trained
#' @param y vector of training scores associated with each document 
#'   in \code{x}
#' @param smooth a smoothing parameter for word counts; defaults to zero for the
#'   to match the LBG (2003) method.
#' @param scale scale on which to score the words; \code{"linear"} for classic 
#'   LBG linear posterior weighted word class differences, or \code{"logit"}
#'   for log posterior differences
#' @details The \code{textmodel_wordscores()} function and the associated
#'   \code{\link[=predict.textmodel_wordscores]{predict()}} method are designed
#'   to function in the same manner as \code{\link[stats]{predict.lm}}.
#'   \code{coef()} can also be used to extract the word coefficients from the
#'   fitted \code{textmodel_wordscore} object, and \code{summary()} will print a
#'   nice summary of the fitted object.
#' @seealso \code{\link{predict.textmodel_wordscores}} for methods of applying a
#'   fitted \link{textmodel_wordscores} model object to predict quantities from
#'   (other) documents.
#' @author Kenneth Benoit
#' @examples 
#' (ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA)))
#' summary(ws)
#' coef(ws)
#' predict(ws)
#' predict(ws, include_reftexts = FALSE)
#' predict(ws, rescaling = "mv")
#' predict(ws, rescaling = "lbg")
#' predict(ws, se.fit = TRUE)
#' predict(ws, se.fit = TRUE, interval = "confidence")
#' predict(ws, se.fit = TRUE, interval = "confidence", rescaling = "mv")
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Extracting Policy Positions From Political Texts Using Words as Data.}" 
#'   \emph{American Political Science Review} 97(02): 311-31
#'   
#'   Beauchamp, N. 2012. "Using Text to Scale Legislatures with Uninformative 
#'   Voting." New York University Mimeo.
#'   
#'   Martin, L W, and G Vanberg. 2007. "A Robust Transformation Procedure for 
#'   Interpreting Political Text." \emph{Political Analysis} 16(1): 93-100.
#' @export
textmodel_wordscores <- function(x, y, scale = c("linear", "logit"), smooth = 0) {
    UseMethod("textmodel_wordscores")
}

#' @export
textmodel_wordscores.default <- function(x, y, scale = c("linear", "logit"), smooth = 0) {
    stop(friendly_class_undefined_message(class(x), "textmodel_wordscores"))
}    

#' @export
textmodel_wordscores.dfm <- function(x, y, scale = c("linear", "logit"), smooth = 0) {
    
    data <- as.dfm(x)
    scores <- y
    scale <- match.arg(scale)
    
    if (nrow(data) < 2)
        stop("wordscores model requires at least two training documents.")
    if (nrow(data) != length(scores))
        stop("trainingdata and scores vector must refer to same number of documents.")
    
    inRefSet <- which(!is.na(scores))
    if (!is.numeric(scores[inRefSet]))
        stop("wordscores model requires numeric scores.")
    
    setscores <- scores[inRefSet] # only non-NA reference texts
    if (smooth) data <- data + 1 # smooth if not 0
    x <- data[inRefSet, ]         # select only the reference texts
    
    Fwr <- dfm_weight(x, "prop")          # normalize words to term frequencies "Fwr"
    tFwr <- t(Fwr)
    Pwr <- tFwr / rowSums(tFwr)    # posterior word probability Pwr
    # compute likelihoods "Pwr" Pr(this word | document)
    if (scale == "linear") {
        Sw <- Pwr %*% setscores
        Sw <- Sw[,1]
    } else if (scale == "logit") {
        if (length(setscores) > 2)
            stop("\nFor logit scale, only two training texts can be used.")
        if (!identical(c(-1,1), sort(setscores))) {
            warning("\nFor logit scale, training scores are automatically rescaled to -1 and 1.")
            scores <- rescaler(setscores)
        }
        lower <- 1
        upper <- 2
        if (setscores[1] > setscores[2]) { lower <- 2; upper <- 1 }
        Sw <- log(Pwr[, upper]) - log(Pwr[, lower])
    }
    
    namesTemp <- names(Sw)
    Sw <- as.vector(Sw[which(colSums(x) > 0)])  # remove words with zero counts in ref set
    names(Sw) <- namesTemp[which(colSums(x) > 0)]
    
    result <- list(
        wordscores = Sw,
        x = data,
        y = scores,
        scale = scale,
        call = match.call()
    )
    class(result) <- c("textmodel_wordscores", "textmodel", "list")
    result
}

#' Predict textmodel_wordscores
#' @param object a fitted Wordscores textmodel
#' @param newdata dfm on which prediction should be made
#' @param se.fit if \code{TRUE}, return standard errors as well
#' @param rescaling \code{"none"} for "raw" scores; \code{"lbg"} for LBG (2003) 
#'   rescaling; or \code{"mv"} for the rescaling proposed by Martin and Vanberg 
#'   (2007).  See References.
#' @param interval type of confidence interval calculation
#' @param level tolerance/confidence level for intervals
#' @param include_reftexts if \code{FALSE}, reference texts are removed from the prediction
#' @param ... not used
#' @return 
#'   \code{textmodel_wordscores()} returns a list that is also classed as a
#'   \code{textmodel_wordscores} object, containing the following elements:
#' @return \item{wordscores}{the scores computed for each word in the training set
#' (\eqn{S_{wd}} from Laver, Benoit and Garry 2003)}
#' @return \item{scale}{either \code{linear} or \code{logit}, according to the value of \code{scale}}
#' @return \item{x}{the dfm on which the wordscores model was called}
#' @return \item{y}{the vector of document reference values}
#' @return \item{call}{the function call that fitted the model}
#'
#' \code{predict.textmodel_wordscores()} returns a named vector of predicted
#' document scores ("text scores" \eqn{S_{vd}} in LBG 2003), or a named list if
#' \code{se.fit = TRUE} consisting of the predicted scores (\code{$fit}) and the
#' associated standard errors (\code{$se.fit}). When \code{interval =
#' "confidence"}, the predicted values will be a matrix.  This behaviour matches
#' that of \code{\link[stats]{predict.lm}}.
#' @keywords textmodel internal
#' @export
#' @importFrom stats qnorm median sd
predict.textmodel_wordscores <- function(object, 
                                         newdata = NULL, 
                                         se.fit = FALSE,
                                         interval = c("none", "confidence"), level = 0.95, 
                                         rescaling = c("none", "lbg", "mv"),
                                         include_reftexts = TRUE,
                                         ...) {
    
    if (length(list(...)) > 0) stop("Arguments: ", names(list(...)), " not supported.\n")
    
    interval <- match.arg(interval)
    rescaling <- match.arg(rescaling)
    
    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }
    
    # Compute text scores as weighted mean of word scores in "virgin" document
    sw <- coef(object)
    data <- dfm_select(data, as.dfm(rbind(sw)))
    # This is different from computing term weights on only the scorable words.
    # It take rowSums() only to generates named vector.
    sw <- sw[intersect(featnames(data), names(sw))]
    raw <- rowSums(dfm_weight(data, "prop") %*% sw)
    
    # if (verbose)
    #    catm(sprintf('%d of %d features (%.2f%%) can be scored\n\n', 
    #         length(sw), nfeat(data), 100 * length(sw) / nfeat(data)))
    
    if (rescaling == "mv") {
        if (sum(!is.na(object$y)) > 2)
            warning("More than two reference scores found with MV rescaling; using only min, max values.")
        fit <- mv_transform(raw, object$y, raw)
    } else if (rescaling == "lbg") {
        lbg_sdr <- stats::sd(object$y, na.rm = TRUE)
        lbg_sv <- mean(raw, na.rm = TRUE)
        lbg_sdv <- if (length(raw) < 2L) 0 else stats::sd(raw)
        lbg_mult <- if (lbg_sdr == 0) 0 else lbg_sdr / lbg_sdv
        fit <- (raw - lbg_sv) * lbg_mult + lbg_sv
    } else {
        fit <- raw
    }
    
    if (!se.fit && interval == "none") {
        if (!include_reftexts) fit <- fit[is.na(object$y)]
        class(fit) <- c("predict.textmodel_wordscores", "numeric")
        return(fit)
    }
    
    # Compute standard error
    raw_se <- rep(NA, length(raw))
    fwv <- dfm_weight(data, "prop")
    for (i in seq_along(raw_se))
        raw_se[i] <- sqrt(sum(as.numeric(fwv[i,]) * (raw[i] - sw) ^ 2)) / sqrt(rowSums(data)[[i]])
    
    result <- list(fit = fit)
    if (se.fit) {
        if (rescaling == "mv") {
            z <- stats::qnorm(1 - (1 - level) / 2)
            upr <- mv_transform(raw + z * raw_se, object$y, raw)
            result$se.fit <- (upr - result$fit) / z
        } else if (rescaling == "lbg") {
            result$se.fit <- (raw_se - lbg_sv) * lbg_mult + lbg_sv
        } else {
            result$se.fit <- raw_se
        }
        
        if (!include_reftexts) result$se.fit <- result$se.fit[is.na(object$y)]
    } 
    
    if (interval == "confidence") {
        # make fit into a matrix
        result$fit <- matrix(result$fit, ncol = 3, nrow = length(result$fit),
                             dimnames = list(names(result$fit), c("fit", "lwr", "upr")))
        
        # Compute confidence intervals
        z <- stats::qnorm(1 - (1 - level) / 2)
        raw <- unname(raw)
        
        if (rescaling == "mv") {
            result$fit[, "lwr"] <- mv_transform(raw - z * raw_se, object$y, raw)
            result$fit[, "upr"] <- mv_transform(raw + z * raw_se, object$y, raw)
        } else if (rescaling == "lbg") {
            if (lbg_mult == 0) {
                result$fit[, "lwr"] <- raw - z * raw_se
                result$fit[, "upr"] <- raw + z * raw_se
            } else {
                result$fit[, "lwr"] <- ((raw - z * raw_se) - lbg_sv) * lbg_mult + lbg_sv
                result$fit[, "upr"] <- ((raw + z * raw_se) - lbg_sv) * lbg_mult + lbg_sv
            }
        } else {
            result$fit[, "lwr"] <- raw - z * raw_se
            result$fit[, "upr"] <- raw + z * raw_se
        }
    } 
    
    # drop the reference texts if needed - handles both vector and matrix case 
    if (!include_reftexts) {
        result$fit <- as.matrix(result$fit)[is.na(object$y), , drop = interval == "none"]
    }
    
    class(result) <- c("predict.textmodel_wordscores", class(result))
    result
}

# internal methods -----------

## Rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min = -1, scale.max = 1) {
    scale.width <- scale.max - scale.min
    scale.factor <- scale.width / (max(x) - min(x))
    return((x - min(x)) * scale.factor - scale.max)
}

## Internal function for MV rescaling
mv_transform <- function(x, y, z) {
    i_low <- which(y == min(y, na.rm = TRUE))
    i_high <- which(y == max(y, na.rm = TRUE))
    return((x - z[i_low]) * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 
               (z[i_high] - z[i_low]) + min(y, na.rm = TRUE))
}

# redefined generic methods -----------

#' @export
#' @method print textmodel_wordscores
#' @noRd
print.textmodel_wordscores <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Scale: ", x$scale, "; ", 
        length(na.omit(x$y)), " reference scores; ",
        length(na.omit(x$wordscores)), " scored features.",
        "\n",
        sep = "")
}

#' @export
#' @noRd
#' @method summary textmodel_wordscores
summary.textmodel_wordscores <- function(object, n = 30L, ...) {

    stat <- data.frame(
        score = object$y,
        total = apply(object$x, 1, sum),
        min = apply(object$x, 1, min),
        max = apply(object$x, 1, max),
        mean = apply(object$x, 1, mean),
        median = apply(object$x, 1, stats::median),
        row.names = docnames(object$x),
        check.rows = FALSE,
        stringsAsFactors = FALSE
    )
    result <- list(
        'call' = object$call,
        'reference.document.statistics' = as.statistics_textmodel(stat),
        'wordscores' = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}


#' @noRd
#' @method coef textmodel_wordscores
#' @export
coef.textmodel_wordscores <- function(object, ...) {
    object$wordscores
}

#' @noRd
#' @method coefficients textmodel_wordscores
#' @export
coefficients.textmodel_wordscores <- function(object, ...) {
    UseMethod("coef")   
}

#' @export
#' @method print predict.textmodel_wordscores
print.predict.textmodel_wordscores <- function(x, ...) {
    print(unclass(x))
}
