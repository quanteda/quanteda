### IN THIS FILE, THE MAIN METHOD and OBJECTS are S4.
### This allows multiple dispatch, esp. for the S4 dfms, but also still
### works on the S3 old-style matrix-based dfm.  
###
### textmodel creates S4 objects now inheriting from textmodel_fitted-class
###
### the print, summary, predict methods are STILL S3 -- which is why they 
### do not dispatch properly unless print() (e.g.) is called specifically, 
### but this could be easily solved by writing appropriate methods in S4.

setClass("textmodel_wordscore_statistics", contains = "data.frame")

#' @rdname textmodel-internal
#' @export
setClass("textmodel_wordscores",
         slots = c(scale = "character", Sw = "numeric"),
         prototype = list(scale = "linear"),
         contains = "textmodel_fitted")

# #' @rdname textmodel-internal
# #' @export
# setClass("textmodel_wordscores_predicted",
#          slots = c(newdata = "dfm", rescaling = "character", level = "numeric",
#                    textscores = "textmodel_wordscore_predictions"),
#          prototype = list(newdata = NULL, rescaling = "none", level = 0.95),
#          contains = "textmodel_wordscores")


#' Wordscores text model
#' 
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension.
#' @param x the \link{dfm} on which the model will be trained
#' @param y vector of training scores associated with each document 
#'   in \code{x}
#' @param smooth a smoothing parameter for word counts; defaults to zero for the
#'   to match the LBG (2003) method.
#' @param scale scale on which to score the words; \code{"linear"} for classic 
#'   LBG linear posterior weighted word class differences, or \code{"logit"}
#'   for log posterior differences
#' @details Fitting a \code{textmodel_wordscores} results in an object of class 
#'   \code{textmodel_wordscores} containing the following slots:
#' @slot scale \code{linear} or \code{logit}, according to the value of 
#'   \code{scale}
#' @slot Sw the scores computed for each word in the training set
#' @slot x  the dfm on which the wordscores model was called
#' @slot y  the reference scores
#' @slot call  the function call that fitted the model
#' @slot method takes a value of \code{wordscores} for this model
#' @section Predict Methods: A \code{predict} method is also available for a 
#'   fitted wordscores object, see 
#'   \code{\link{predict.textmodel_wordscores}}.
#' @author Kenneth Benoit
#' @examples 
#' ws <- textmodel_wordscores(data_dfm_lbgexample, c(seq(-1.5, 1.5, .75), NA))
#' 
#' summary(ws)
#' predict(ws)
#' predict(ws, rescaling = "mv")
#' predict(ws, rescaling = "lbg")
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#'   "\href{http://www.kenbenoit.net/pdfs/WORDSCORESAPSR.pdf}{Extracting Policy Positions From Political Texts Using Words as Data.}" 
#'   \emph{American Political Science Review} 97(02): 311-31
#'   
#'   Beauchamp, N. 2012. "Using Text to Scale Legislatures with Uninformative 
#'   Voting." New York University Mimeo.
#'   
#'   Martin, L W, and G Vanberg. 2007. "A Robust Transformation Procedure for 
#'   Interpreting Political Text." \emph{Political Analysis} 16(1): 93-100.
#' @seealso \code{\link{predict.textmodel_wordscores}}
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
    if (scale=="linear") {
        Sw <- Pwr %*% setscores
        Sw <- Sw[,1]
    } else if (scale=="logit") {
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
    
    new("textmodel_wordscores", Sw = Sw, x = data, y = scores, 
        method = "wordscores", scale = scale, call = match.call())
}


#' @rdname textmodel-internal
#' @param object a fitted Wordscores textmodel
#' @param level probability level for confidence interval width
#' @param rescaling \code{"none"} for "raw" scores; \code{"lbg"} for LBG (2003) 
#'   rescaling; or \code{"mv"} for the rescaling proposed by Martin and Vanberg 
#'   (2007).  (Note to authors: Provide full details here in documentation.)
#' @param newdata dfm on which prediction should be made
#' @param ... additional arguments passed to other functions
#' @param verbose If \code{TRUE}, output status messages
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#' "Extracting Policy Positions From Political Texts Using Words as Data." 
#' \emph{American Political Science Review} 97(02): 311-31.
#' 
#' Martin, L W, and G Vanberg. 2007. "A Robust Transformation Procedure for
#' Interpreting Political Text." \emph{Political Analysis} 16(1): 93-100.
#' @return The \code{predict} method for a wordscores fitted object returns a 
#'   data.frame whose rows are the documents fitted and whose columns contain 
#'   the scored textvalues, with the number of columns depending on the options 
#'   called (for instance, how many rescaled scores, and whether standard errors
#'   were requested.)  (Note: We may very well change this soon so that it is a 
#'   list similar to other existing fitted objects.)
#' @keywords internal textmodel
#' @export
#' @importFrom stats qnorm median sd
predict.textmodel_wordscores <- function(object, 
                                         newdata = NULL, 
                                         se.fit = FALSE,
                                         interval = c("none", "confidence"), level = 0.95, 
                                         rescaling = c("none", "lbg", "mv"),
                                         ...) {
    
    if (length(list(...)) > 0) 
        stop("Arguments:", names(list(...)), "not supported.\n")
        
    interval <- match.arg(interval)
    rescaling <- match.arg(rescaling)
        
    if (!is.null(newdata))
        data <- as.dfm(newdata)
    else {
        data <- as.dfm(object@x)
    }
    
    # Compute text scores as weighted mean of word scores in "virgin" document
    data <- dfm_select(data, as.dfm(rbind(object@Sw)))
    # This is different from computing term weights on only the scorable words.
    # It take rowSums() only to generates named vector.
    sw <- object@Sw[intersect(featnames(data), names(object@Sw))]
    raw <- rowSums(dfm_weight(data, "prop") %*% sw)
    
    # if (verbose)
    #    catm(sprintf('%d of %d features (%.2f%%) can be scored\n\n', 
    #         length(sw), nfeat(data), 100 * length(sw) / nfeat(data)))
    
    if (rescaling == "mv") {
        if (sum(!is.na(object@y)) > 2)
            warning("More than two reference scores found with MV rescaling; using only min, max values.")
        result <- list(fit = mv_transform(raw, object@y, raw))
    } else if (rescaling == "lbg") {
        lbg_sdr <- stats::sd(object@y, na.rm = TRUE)
        lbg_sv <- mean(raw, na.rm = TRUE)
        lbg_sdv <- if (length(raw) < 2L) 0 else stats::sd(raw)
        lbg_mult <- if (lbg_sdr == 0) 0 else lbg_sdr / lbg_sdv
        result <- list(fit = (raw - lbg_sv) * lbg_mult + lbg_sv)
    } else {
        result <- list(fit = raw)
    }
    
    if (!se.fit && interval == 'none')
        return(result[[1]])    
    
    # Compute standard error
    raw_se <- rep(NA, length(raw))
    fwv <- dfm_weight(data, "prop")
    for (i in seq_along(raw_se))
        raw_se[i] <- sqrt(sum(as.numeric(fwv[i,]) * (raw[i] - sw) ^ 2)) / sqrt(rowSums(data)[[i]])
    
    if (interval == 'none') {
        
        if (rescaling == "mv") {
            result$se <- rep(NA, length(raw))
        } else if (rescaling == "lbg") {
            result$se <- raw_se
        } else {
            result$se <- raw_se
        }
        return(result)
        
    } else {
    
        # Compute confidence intervals
        z <- stats::qnorm(1 - (1 - level) / 2)
        raw <- unname(raw)
        
        if (rescaling == "mv") {
            result$lo <- mv_transform(raw - z * raw_se, object@y, raw)
            result$hi <- mv_transform(raw + z * raw_se, object@y, raw)
        } else if (rescaling == "lbg") {
            if (lbg_mult == 0) {
                result$lwr <- raw - z * raw_se
                result$upr <- raw + z * raw_se
            } else {
                result$lwr <- ((raw - z * raw_se) - lbg_sv) * lbg_mult + lbg_sv
                result$upr <- ((raw + z * raw_se) - lbg_sv) * lbg_mult + lbg_sv
            }
        } else {
            result$lwr <- raw - z * raw_se
            result$upr <- raw + z * raw_se
        }
        return(as.matrix(as.data.frame(result)))
        
    }
}


## Rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min = -1, scale.max = 1) {
    scale.width <- scale.max - scale.min
    scale.factor <- scale.width / (max(x) - min(x))
    return((x - min(x)) * scale.factor - scale.max)
}

## Internal function for MV rescaling
mv_transform <- function(x, y, z) {
    y <- y[!is.na(y)]
    i_low <- which(y == min(y))
    i_high <- which(y == max(y))
    return((x - z[i_low]) * (max(y) - min(y)) / (z[i_high] - z[i_low]) + min(y))
}


#' @rdname textmodel-internal
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print
#' @param digits number of decimal places to print for print methods
#' @export
#' @method print textmodel_wordscores
print.textmodel_wordscores <- function(x, digits = 2, ...) {
    cat("Fitted wordscores model:\n")
    cat("Call:\n")
    print(x@call)
    cat("\n")
    cat("Reference Documents and Reference Scores:\n\n")
    temp <- data.frame(Document = docnames(x@x),
                       Score = x@y, 
                       stringsAsFactors = FALSE)
    print(temp, digits = digits, row.names = FALSE)
    cat("\n")
}

#' @rdname textmodel-internal
#' @keywords internal
#' @export
setMethod("show", signature(object = "textmodel_wordscores"), 
          function(object) print(object))

# #' @rdname textmodel-internal
# #' @keywords internal
# #' @export
# setMethod("show", signature(object = "textmodel_wordscores_predicted"), 
#           function(object) print(object))


#' @export
#' @noRd
#' @method summary textmodel_wordscores
summary.textmodel_wordscores <- function(object, n = 30L, ...) {

    temp <- data.frame(Document = docnames(object@x),
                       Score = object@y,
                       Total = apply(object@x, 1, sum),
                       Min = apply(object@x, 1, min),
                       Max = apply(object@x, 1, max),
                       Mean = apply(object@x, 1, mean),
                       Median = apply(object@x, 1, stats::median),
                       stringsAsFactors = FALSE)
    result <- list('call' = object@call,
                   'reference document statistics' = new('textmodel_wordscore_statistics', temp),
                   'word scores' = new('textmodel_coefficients', head(object@Sw, n)))
    new('textmodel_summary', result)
}

#' @rdname textmodel-internal
#' @export
setMethod("coefficients", signature(object = "textmodel_wordscores"),
          function(object, ...) cbind('Estimate' = object@Sw))

#' @rdname textmodel-internal
#' @export
setMethod("coef", signature(object = "textmodel_wordscores"),
          function(object, ...) cbind('Estimate' = object@Sw))

#' Impliments print methods for textmodel_wordscore_statistics 
#'
#' @param x a textmodel_wordscore_statistics object
#' @param ... additional arguments not used
#' @export
print.textmodel_wordscore_statistics <- function(x, digits = digits, ...) {
    cat("(reference scores and feature count statistics)\n\n")
    NextMethod(digits = digits, row.names = FALSE)
}

# #' Impliments print methods for textmodel_wordscore_predictions
# #'
# #' @param x a textmodel_wordscore_predictions object
# #' @param ... additional arguments not used
# #' @export
# print.textmodel_wordscore_predictions <- function(x, ...) {
#     
#     label <- c('raw'= 'textscore',
#                'raw_se' = 'LBG se',
#                'raw_lo' = 'ci lo',
#                'raw_hi' = 'ci hi',
#                'mv' = 'MV rescaled',
#                'mv_lo' = 'MV lo',
#                'mv_hi' = 'MV hi',
#                'lbg' = 'LBG rescaled',
#                'lbg_lo' = 'LBG lo',
#                'lbg_hi' = 'LBG hi')
#     
#     # pare down the output if rescaling has been specified
#     #if (any(c("lbg", "mv") %in% names(x)))
#     #    x$raw_lo <- x$raw_hi <- NULL
#     names(x) <- label[names(x)]
#     print(round(x, 4))
#     cat('\n')
# }
