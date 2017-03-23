
#' apply a dictionary or thesaurus to an object
#' 
#' Convert features into equivalence classes defined by values of a dictionary 
#' object.
#' @note Selecting only features defined in a "dictionary" is traditionally 
#'   known in text analysis as a \emph{dictionary method}, even though
#'   technically this "dictionary" operates more like a thesarus.  If a thesaurus-like
#'   application is desired, set \code{exclusive = FALSE} to convert features 
#'   defined as values in a dictionary into their keys, while keeping all other
#'   features.
#' @return an object of the type passed with the value-matching features
#'   replaced by dictionary keys
#' @param x object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to
#'   \code{x}
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param capkeys if \code{TRUE}, convert dictionary keys to
#'   uppercase to distinguish them from other features
#' @param verbose print status messages if \code{TRUE}
#' @param ... not used
#' @keywords internal deprecated
#' @export
applyDictionary <- function(x, dictionary, ...) {
    UseMethod("applyDictionary")
}


#' @rdname applyDictionary
#' @examples 
#' 
#' toks <- tokens(data_corpus_inaugural)
#' head(kwic(toks, "united states"))
#' dict <- dictionary(list(country = "united states"))
#' toks2 <- applyDictionary(toks, dict, valuetype = "fixed")
#' toks2
#' 
#' @export 
applyDictionary.tokens <- function(x, ...) {
    .Deprecated("tokens_lookup")
    tokens_lookup(x, ...)
}

#' @rdname applyDictionary
#' @export 
applyDictionary.tokenizedTexts <- function(x, ...) {
    .Deprecated("tokens_lookup")
    tokens_lookup(as.tokens(x), ...)
}

#' @rdname applyDictionary
#' @details \code{applyDictionary.dfm} is the deprecated function name for
#' \code{\link{dfm_lookup}}.
#' @export
applyDictionary.dfm <- function(x, ...) {
    .Deprecated("dfm_lookup")
    dfm_lookup(x, ...)
}

