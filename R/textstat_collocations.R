#' calculate collocation statistics
#' 
#' @rdname textstat_collocations
#' @param x a \link{dfm} containing the features to be examined for keyness
#' @param target the document index (numeric, character or logical) identifying the 
#'   document forming the "target" for computing keyness; all other documents' 
#'   feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness.
#'   Currenly available: \code{"chi2"} (\eqn{chi^2} with Yates correction); 
#'   \code{"exact"} (Fisher's exact test); \code{"lr"} for the likelihood ratio
#'   \eqn{G} statistic with Yates correction.
#' @param sort logical; if \code{TRUE} sort features scored in descending order 
#'   of the measure, otherwise leave in original feature order
#' @export
#' @keywords textstat
#' @examples
#' toks <- tokens(data_char_inaugural)
#' toks <- tokens_remove(toks, stopwords())
#' coxs <- contexts(toks, "clinton", window = 10, valuetype = "glob")
#' textstat_collocations(coxs, toks)
textstat_collocations <- function(x, y, ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.dfm <- function(x, y, ...) {
    
    z <- dfm_select(x, y, padding = TRUE)
    textstat_keyness(new("dfmSparse", Matrix::rbind2(z, y)), target = 1:nrow(x), ...)
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, y, ...) {
    
    textstat_collocations(dfm(x), dfm(y), ...)
}

