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
#'   text. #' @importFrom stringi stri_split_fixed stri_split_boundaries 
#'   stri_trim_right
#' @importFrom parallel detectCores
#' @export
#' @examples 
#' # same for character vectors and for lists
#' str(toLower(ukimmigTexts))
#' str(tokenize(ukimmigTexts, toLower = FALSE))
#' str(toLower(tokenize(ukimmigTexts, toLower = FALSE)))
toLower <- function(x, ...) {
    UseMethod("toLower")
}

#' @rdname toLower
#' @export
toLower.character <- function(x, cores = parallel::detectCores(), ...) {
    # Windows cannot use  mclapply
    if (.Platform$OS.type == "windows") cores <- 1
    res <- parallel::mclapply(x, stri_trans_tolower, mc.cores = cores, ...)
    # return a character vector
    res <- simplify2array(res)
    names(res) <- names(x)
    return(res)
}

# tokenize could return a vector

#' @rdname toLower
#' @export
toLower.list <- function(x, cores = parallel::detectCores(), ...){
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    # Windows cannot use  mclapply
    if (.Platform$OS.type == "windows") cores <- 1
    # parallelize toLower call but don't recursively parallelize individual threads
    mclapply(x, toLower, mc.cores = cores, cores = 1)
}

