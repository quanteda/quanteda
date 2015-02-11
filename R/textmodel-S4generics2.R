### IN THIS FILE, THE MAIN METHOD and OBJECTS are S4.
### This allows multiple dispatch, esp. for the S4 dfms, but also still
### works on the S3 old-style matrix-based dfm.  
###
### textmodel2 creates S4 objects now.  The methods for print, 
### summary, etc. are now more specific and defined in the model-specific
### R files (see textmodel-wordscoresS4.R.


#' the fitted textmodel classes
#' 
#' The \code{textmodel} virtual class is a parent class for more specific fitted text models,
#' which are the result of a quantitative text analysis applied to a document-feature
#' matrix.
#' 
#' Available types currently include...
#' @slot dfm a \link{dfm-class} document-feature matrix
#' @name textmodel_fitted-class
#' @export
setClass("textmodel_fitted",
         slots = c(x = "dfm", y = "ANY", call = "call", method = "character"))


#' fit a text model
#' 
#' Fit a text model to a dfm.  Creates an object of virtual class 
#' \link{textmodel_fitted-class}, whose exact properties (slots and methods)
#' will depend on which model was called (see \code{model} types below).
#' @param x a quanteda \link{dfm} object containing feature counts by document
#' @param y for supervised models, a vector of class labels or values for 
#'   training the model, with \code{NA} for documents to be excluded from the 
#'   training set; for unsupervised models, this will be left NULL
#' @param model the model type to be fit.  Currently implemented methods are: 
#'   \describe{ \item{\code{wordscores}}{Fits the "wordscores" model of Laver, 
#'   Benoit, and Garry (2003). Options include the original linear scale of LBG 
#'   or the logit scale proposed by Beauchamps (2001).  See 
#'   \link{textmodel_wordscores}.}
#'   
#'   \item{\code{NB}}{Fits a Naive Bayes model to the dfm, with options for 
#'   smoothing, setting class priors, and a choice of multinomial or binomial 
#'   probabilities.}}
#' @return a \code{textmodel} class list, containing the fitted model and 
#'   additional information specific to the model class.  See the methods for 
#'   specific models, e.g. \link{textmodel_wordscores}, \link{textmodel_NB}, 
#'   etc.
#' @param ... additional arguments to be passed to specific model types
#' @seealso \code{\link{textmodel_wordscores}}, \code{\link{textmodel_NB}}, 
#'   \code{\link{textmodel_wordfish}}, \code{\link{textmodel_lda}}, 
#'   \code{\link{textmodel}}
#' @section Class hierarchy: Here will go the description of the class hierarchy
#'   that governs dispatch for the predict, print, summary methods, since this 
#'   is not terribly obvious. (Blame it on the S3 system.)
#' @export
#' @examples
#' data(ie2010Corpus, package="quantedaData")
#' \dontshow{
#' # test with old-style matrix-based dense dfm
#' ieDfmd <- dfm(ie2010Corpus, matrixType="dense", verbose=FALSE)
#' refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
#' ws <- textmodel2(ieDfmd, refscores, model="wordscores", smooth=1)
#' ws <- textmodel2(ieDfmd, refscores, model="wordscores")
#' ws <- textmodel2(refscores ~ . -1, data=ieDfmd, model="wordscores")
#' rm(ieDfmd)
#' }
#' ieDfm <- dfm(ie2010Corpus, verbose=FALSE)
#' refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
#' ws <- textmodel2(ieDfm, refscores, model="wordscores", smooth=1)
#' # alternative formula notation - but slower
#' wsform <- textmodel2(refscores ~ . - 1, data=ieDfm, model="wordscores", smooth=1)
#' identical(ws@@Sw, wsform@@Sw)  # compare wordscores from the two models
#' bs <- textmodel2(ieDfm[5:6,], refscores[5:6], model="wordscores", scale="logit", smooth=1)
#' plot(ws@@Sw, bs@@Sw, xlim=c(-1, 1), xlab="Linear word score", ylab="Logit word score")
#' 
#' @export
setGeneric("textmodel2", 
    function(x, y=NULL, data=NULL, model=c("wordscores", "NB", "wordfish", "lda", "ca"), ...)
             standardGeneric("textmodel2"))

#' @rdname textmodel2
setMethod("textmodel2", signature(x = "dfm", y="ANY", data="missing", model = "character"),
          definition = 
              function(x, y=NULL, model=c("wordscores", "NB", "wordfish", "lda", "ca"), ...) {
                  #cat("x is:"); print(x)
                  #cat("y is:"); print(y)
                  #cat("model is:", model, "\n")
                  model <- match.arg(model)
                  call <- match.call()
                  Yname <- deparse(substitute(y))
                  
                  if (model=="wordscores") {
                      if (nrow(x) != length(y))
                          stop("x and y contain different numbers of documents.")
                      
                      result <- textmodel2_wordscores(x, y, ...)
                  } else if (model=="NB") {
                      if (nrow(x) != length(y))
                          stop("x and y contain different numbers of documents.")
                      result <- textmodel_NB(x, y, ...)
                  } else if (model=="wordfish") {
                      if (!is.null(y))
                          warning("y values not used with wordfish model. ")
                      result <- textmodel_wordfish(x, ...)
                  } else if (model=="lda") {
                      if (!is.null(y))
                          warning("y values not used with wordfish model. ")
                      result <- textmodel_lda(x, ...)
                  } else if (model=="ca") {
                      if (!is.null(y))
                          warning("y values not used with ca model. ")
                      result <- textmodel_lda(x, ...)
                  } else {
                      stop(paste("model", method, "not implemented."))
                  }
                  result
              })


#' @rdname textmodel2
#' @param formula An object of class \link{formula} of the form \code{y ~ x1
#'   + x2 + ...}.  (Interactions are not currently allowed for any of the models
#'   implemented.)  The \code{x} variable(s) is
#'   typically a \link{dfm}, and the y variable a vector of class labels or 
#'   training values associated with each document.
#' @param data dfm or data.frame from which to take the formula
setMethod("textmodel2", signature(x = "formula", y="missing", data="dfm", model = "character"),
          definition = 
              function(x, data, model=c("wordscores", "NB", "wordfish", "lda", "ca"), ...) {
                  model <- match.arg(model)
                  if (isS4(data))  # is it a new type dfm class object
                      mf <- model.frame(formula=x, data=as.data.frame(as.matrix(data)))
                  else 
                      mf <- model.frame(formula=x, data=data)
                  
                  # x <- model.matrix(attr(mf, "terms"), data=mf)
                  y <- model.response(mf)
                  # cat("HERE\n")
                  textmodel2(data[which(docnames(data) %in% names(y)), 
                                 which(features(data) %in% names(mf))], 
                            y, model = model, ...)
              })

