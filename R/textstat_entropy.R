#' Compute entropies of documents or features
#'
#' @param x a `dfm`
#' @param margin character indicating for which margin to compute entropy
#' @param base base for logarithm function
#' @return a data.frame of entropies for the given document or feature
#' @export
#' @examples
#' textstat_entropy(data_dfm_lbgexample)
#' textstat_entropy(data_dfm_lbgexample, "features")
textstat_entropy <- function(x, margin = c("documents", "features"), base = 2) {
    UseMethod("textstat_entropy")
}

#' @export
textstat_entropy.default <- function(x, margin = c("documents", "features"), base = 2) {
    stop(friendly_class_undefined_message(class(x), "textstat_entropy"))
}

#' @export
textstat_entropy.dfm <- function(x, margin = c("documents", "features"), base = 2) {
    x <- as.dfm(x)
    margin <- match.arg(margin)
    if (margin == "features")
        x <- t(x)
    x <- dfm_weight(x, "prop")
    x <- as(x, "dgTMatrix")
    e <- unlist(lapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                       function(y) sum(y * log(y, base)) * -1), use.names = FALSE)
    result <- data.frame(rownames(x), e, stringsAsFactors = FALSE)
    names(result) <- c(stri_sub(margin, 1, -2), "entropy")
    class(result) <- c("entropy", "textstat", "data.frame")
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}
