#' Compute entropy of documents or features
#' 
#' @param x a \code{dfm}
#' @param margin character indicating for which margin to compute entropy
#' @param base base for logarithm function
#' @export
#' @examples 
#' textstat_entropy(data_dfm_lbgexample)
#' textstat_entropy(data_dfm_lbgexample, "features")
textstat_entropy <- function(x, margin = c("documents", "features"), base = 2) {
    
    x <- as.dfm(x)
    margin <- match.arg(margin)
    if (margin == "features")
        x <- t(x)
    x <- dfm_weight(x, "prop")
    x <- as(x, "dgTMatrix")
    result <- unlist(lapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                            function(y) sum(y * log(y, base)) * -1), use.names = FALSE)
    names(result) <- rownames(x)
    return(result)
}
