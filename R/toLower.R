#' Convert texts to lower case
#' 
#' Convert texts to lower case
#' @rdname toLower
#' @param x text to be lower-cased
#' @param cores number of \code{mc.cores} to use in \pkg{parallel} processing 
#'   operations, default is the (logical) system cores returned by
#'   \code{link[parallel]{detectCores}}
#' @param ... additional arguments passed to \code{\link{stri_trans_tolower}}, 
#'   such as \code{locale}
#' @return A list of length \code{\link{ndoc}(x)} of the tokens found in each 
#'   text. 
#' @importFrom stringi stri_split_fixed stri_split_boundaries 
#'   stri_trim_right
#' @importFrom parallel detectCores
#' @export
#' @examples 
#' test1 <- c('I joined NATO and UNESCO', 'and NASA')
#' toLower(test1, keepAcronyms=TRUE)
toLower <- function(x, ...) {
    UseMethod("toLower")
}

toLowerSingleKeep <- function(x){
    res <- c()
    if(stri_detect_regex(x, '(\\b[A-Z]{2,}\\b)')){
        tmp <- stri_replace_all_regex(x, '(\\b[A-Z]{2,}\\b)',  '_$1_')
        res <- stri_trans_tolower(tmp)
        m1 <- stri_match_all_regex(res, '(\\b_[a-z]+_\\b)')[[1]][,1]
        m2 <- stri_trans_toupper(m1)
        res <- stri_replace_all_regex(res, m1,  m2, vectorize_all = FALSE)
    }else{
        res <- stri_trans_tolower(x)
    }
    return(res)
    
}

#' @rdname toLower
#' @export
toLower.character <- function(x, keepAcronyms=FALSE, ...) {
    res <- c()
    if(keepAcronyms){
        res <- lapply(x, toLowerSingleKeep)
    }else{
        res <- stri_trans_tolower(x)
    }
    names(res) <- names(x)
    return(res)
}



#' @rdname toLower
#' @export
toLower.list <- function(x, keepAcronyms=FALSE,  ...){
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    return(lapply(x, toLower, keepAcronyms=keepAcronyms))
    }
