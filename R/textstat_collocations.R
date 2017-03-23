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
#' 
#' toks <- tokens(data_char_inaugural, removePunct = TRUE)
#' toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#' head(textstat_collocations(toks, method = 'bj'))
#' head(textstat_collocations(toks, method = 'lr'))
#' 
#' 
textstat_collocations <- function(x, ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, method = 'lr', ...) {
    
    if (method == 'bj') {
        result <- sequences.tokens(x, ...)
    } else {
        result <- collocations.tokens(x, method, ...)
    }
    class(result) <- c("collocation", 'data.frame')
    return(result)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.corpus <- function(x, method, ...) {
    textstat_collocations(texts(x), ...)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.character <- function(x, method, ...) {
    textstat_collocations(tokens(x, ...), ...)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.tokenizedTexts <- function(x, ...) {
    textstat_collocations(as.tokens(x), ...)
}


#' @method "[" collocation
#' @export
#' @noRd
"[.collocation" <- function(x, i, ...) {
    x <- as.data.frame(x)[i,]
    attr(x, 'ids') <- attr(x, 'ids')[i]
    class(x) <- c("collocation", 'data.frame')
    return(x)
}

#' @export
#' @method as.tokens collocation
#' @noRd 
as.tokens.collocation <- function(x) {
    toks <- attr(x, 'ids')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}


