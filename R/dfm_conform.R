#' Make features of a dfm identical
#' @param x a dfm
#' @param features character vectors of features or a \code{dfm}
#' @export
dfm_conform <- function(x, features) {
    x <- as.dfm(x)
    if (is.dfm(features))
        features <- featnames(features)
    if (!is.character(features))
        stop("Features must be a character vector or dfm")
    attrs <- attributes(x)
    x <- pad_dfm(x, features)
    x <- set_dfm_slots(x, attrs)
    return(x)
}