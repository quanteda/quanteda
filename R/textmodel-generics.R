### IN THIS FILE, THE MAIN METHOD and OBJECTS are S4.
### This allows multiple dispatch, esp. for the S4 dfms, but also still
### works on the S3 old-style matrix-based dfm.  
###
### textmodel creates S4 objects now.  The methods for print, 
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
#'   training set; for unsupervised models, this will be left \code{NULL}.
#' @param model the model type to be fit.  Currently implemented methods are: 
#'   \describe{ \item{\code{wordscores}}{Fits the "wordscores" model of Laver, 
#'   Benoit, and Garry (2003). Options include the original linear scale of LBG 
#'   or the logit scale proposed by Beauchamps (2001).  See 
#'   \link{textmodel_wordscores}.}
#'   
#'   \item{\code{NB}}{Fits a Naive Bayes model to the dfm, with options for 
#'   smoothing, setting class priors, and a choice of multinomial or binomial 
#'   probabilities.  See \link{textmodel_NB}.}
#'   
#'   \item{\code{wordfish}}{Fits the "wordfish" model of Slapin and Proksch (2008).  
#'   See \link{textmodel_wordfish}.}
#'   
#'   \item{\code{ca}}{Correspondence analysis scaling of the dfm.}
#'   
#'   \item{\code{lda}}{Fit a topic model based on latent Dirichlet allocation.  
#'   Not yet implemented -- use \code{\link{convert}}
#'   to convert a dfm into the applicable input format and then use your favourite 
#'   topic modelling package directly.}
#'   
#'   \item{\code{kNN}}{k-nearest neighbour classification, coming soon.}
#'   }
#' @return a \code{textmodel} class list, containing the fitted model and 
#'   additional information specific to the model class.  See the methods for 
#'   specific models, e.g. \link{textmodel_wordscores}, 
#'   etc.
#' @param ... additional arguments to be passed to specific model types
#' @seealso \code{\link{textmodel}}, \code{\link{textmodel_wordscores}}
# , \code{\link{textmodel_NB}}, 
#   \code{\link{textmodel_wordfish}}, \code{\link{textmodel_lda}}, 
#' @section Class hierarchy: Here will go the description of the class hierarchy
#'   that governs dispatch for the predict, print, summary methods, since this 
#'   is not terribly obvious. (Blame it on the S3 system.)
#' @export
#' @examples
#' ieDfm <- dfm(ie2010Corpus, verbose=FALSE)
#' refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
#' ws <- textmodel(ieDfm, refscores, model="wordscores", smooth=1)
#'
#' # alternative formula notation - but slower
#' # need the - 1 to remove the intercept, as this is literal formula notation
#' wsform <- textmodel(refscores ~ . - 1, data=ieDfm, model="wordscores", smooth=1)
#' identical(ws@Sw, wsform@Sw)  # compare wordscores from the two models
#' 
#' 
#' # compare the logit and linear wordscores
#' bs <- textmodel(ieDfm[5:6,], refscores[5:6], model="wordscores", scale="logit", smooth=1)
#' plot(ws@@Sw, bs@@Sw, xlim=c(-1, 1), xlab="Linear word score", ylab="Logit word score")
#' 
#' \dontrun{wf <- textmodel(ieDfm, model="wordfish", dir = c(6,5))
#' wf}
#' @export
setGeneric("textmodel", 
    function(x, y=NULL, data=NULL, model=c("wordscores", "NB", "wordfish", "ca"), ...)
             standardGeneric("textmodel"))

#' @rdname textmodel
setMethod("textmodel", signature(x = "dfm", y="ANY", data="missing", model = "character"),
          definition = 
              function(x, y=NULL, model=c("wordscores", "NB", "wordfish", "ca"), ...) {
                  #catm("x is:"); print(x)
                  #catm("y is:"); print(y)
                  #catm("model is:", model, "\n")
                  model <- match.arg(model)
                  call <- match.call()
                  Yname <- deparse(substitute(y))
                  
                  if (model=="wordscores") {
                      if (ndoc(x) != length(y))
                          stop("x and y contain different numbers of documents.")
                      result <- textmodel_wordscores(x, y, ...)
                  } else if (model=="wordfish") {
                      if (!is.null(y))
                          warning("y values not used with wordfish model. ")
                      result <- textmodel_wordfish(x, ...)
                  } else if (model=="NB") {
                      if (nrow(x) != length(y))
                          stop("x and y contain different numbers of documents.")
                      result <- textmodel_NB(x, y, ...)
#                   } else if (model=="lda") {
#                       if (!is.null(y))
#                           warning("y values not used with wordfish model. ")
#                       result <- textmodel_lda(x, ...)
                  } else if (model=="ca") {
                      if (!is.null(y))
                          warning("y values not used with ca model. ")
                      result <- textmodel_ca(x, ...)
                  } else {
                      stop(paste("model", model, "not implemented."))
                  }
                  result
              })


#' @rdname textmodel
#' @param formula An object of class \link{formula} of the form \code{y ~ x1
#'   + x2 + ...}.  (Interactions are not currently allowed for any of the models
#'   implemented.)  The \code{x} variable(s) is
#'   typically a \link{dfm}, and the y variable a vector of class labels or 
#'   training values associated with each document.
#' @param data dfm or data.frame from which to take the formula
#' @importFrom stats model.frame model.response
setMethod("textmodel", signature(x = "formula", y="missing", data="dfm", model = "character"),
          definition = 
              function(x, data, model=c("wordscores", "NB", "wordfish", "lda", "ca"), ...) {
                  model <- match.arg(model)
                  if (isS4(data))  # is it a new type dfm class object
                      mf <- stats::model.frame(formula=x, data=as.data.frame(as.matrix(data)))
                  else 
                      mf <- stats::model.frame(formula=x, data=data)
                  
                  # x <- model.matrix(attr(mf, "terms"), data=mf)
                  y <- stats::model.response(mf)
                  # catm("HERE\n")
                  textmodel(data[which(docnames(data) %in% names(y)), 
                                 which(features(data) %in% names(mf))], 
                            y, model = model, ...)
              })

