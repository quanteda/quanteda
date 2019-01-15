#' Make features of a dfm identical
#' @param x a dfm
#' @param features character vectors of features
#' @export
dfm_match <- function(x, features) {
    x <- as.dfm(x)
    if (!is.character(features))
        stop("Features must be a character vector")
    attrs <- attributes(x)
    x <- pad_dfm(x, features)
    x <- set_dfm_slots(x, attrs)
    return(x)
}