# classes -----------------------

#' The fitted textmodel classes
#' 
#' The \code{textmodel} virtual class is a parent class for more specific fitted text models,
#' which are the result of a quantitative text analysis applied to a document-feature
#' matrix.
#' 
#' Available types currently include...
#' @slot x  the dfm on which the wordscores model was called
#' @slot y  supervised labels (for supervised methods only)
#' @slot call  the function call that fitted the model
#' @slot method the type of textmodel, e.g. wordfish, wordscores, nb
#' @name textmodel_fitted-class
#' @keywords internal textmodel
#' @export
setClass("textmodel_fitted",
         slots = c(x = "dfm", y = "ANY", call = "call", method = "character"))

# convenience methods  ----------- 

#' Extract text model coefficients
#' 
#' Extract text model coefficients for documents and features, in a manner
#' similar to \link{coef} and \link{coefficients}. (\code{coefficients} is an
#' alias for \link{coef}.)
#' @param object a fitted or predicted text model object whose coefficients will
#'   be extracted
#' @param ... unused
#' @name coef.textmodel
#' @return 
#' Returns a list of named numeric vectors with the following elements:
#' \describe{
#' \item{\code{coef_feature}}{coefficients estimated for each feature}
#' \item{\code{coef_feature_se}}{standard errors estimated for each
#' feature-level point estimate}
#' \item{\code{coef_document}}{coefficients estimated for each document}
#' \item{\code{coef_document_se}}{standard errors estimated for each
#' document-level point estimate}
#' \item{\code{coef_document_offset}}{a document-level offset for applicable
#' models} 
#' \item{\code{coef_feature_offset}}{a feature-level offset for applicable
#' models}
#' }
#' An element that is not applicable for a particular object class will be
#' \code{NULL}, for instance \code{coef_documents} has no meaning for a fitted
#' wordscores object.
coef.textmodel <- coefficients.textmodel <- function(object, ...) {
    coef(object, ...)
}

# internal methods ----------- 

#' Internal functions for textmodel objects
#' 
#' Internal function documentation for textmodel objects.
#' @name textmodel-internal
#' @keywords internal 
NULL
