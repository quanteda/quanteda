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
#' lowerTexts <- toLower('A single Character vector.')
#' 
#' toks <- tokenize(ukimmigTexts)
#' lowerToks <- toLower(toks)
#' str(tokensFromChar)
#' 
toLower <- function(x, ...) {
    UseMethod("toLower")
}


toLower.character <- function(x, ...){
    res <- stri_trans_tolower(x, locale = NULL)
    names(res) <- names(x)
    return(res)
}

# tokenize could return a vector

toLower.list <- function(x, ...){
    typeTest <- all(sapply(x, is.character))
    if(!typeTest){
        stop("Each element of the list must be a character vector.")
    }
    result <- lapply(x, toLower)
}

