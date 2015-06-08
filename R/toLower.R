#' Convert texts to lower case
#'
#' Convert texts to lower case
#' @rdname toLower
#' @param x The text to be lower-cased
#' @param ... additional arguments passed to \code{\link{clean}}
#' @return A list of length \code{\link{ndoc}(x)} of the tokens found in each text.
#' #' @importFrom stringi stri_split_fixed stri_split_boundaries stri_trim_right
#' @export
#' @examples 
#' # same for character vectors and for lists
#' tokensFromChar <- tokenize(inaugTexts[1:3])
#' tokensFromCorp <- tokenize(subset(inaugCorpus, Year<1798))
#' identical(tokensFromChar, tokensFromCorp)
#' str(tokensFromChar)
#' 
toLower <- function(x, ...) {
    UseMethod("toLower")
}


toLower.character <- function(x, ...){
    return(stri_trans_tolower(x, locale = NULL))
}


toLower.list <- function(x, ...){
    result <- sapply(x, toLower)
}