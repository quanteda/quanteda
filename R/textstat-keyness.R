#' calculate keyness statistics
#' 
#' @param x a dfm in which the first row is the target and the second is the reference document.
#' @references Bondi, Marina, and Mike Scott, eds. Keyness in Texts. Amsterdam, 
#'   Philadelphia: John Benjamins, 2010.
#'   Stubbs, Michael. ‘Three Concepts of Keywords’. In Keyness in Texts, edited 
#'   by Marina Bondi and Mike Scott, 21–42. Amsterdam, Philadelphia: John Benjamins, 2010.
#' @return a named vector of keyness scores
#' @export
# @examples
# period <- as.factor(ifelse(docvars(data_corpus_inaugural)$Year < 1945, 'before', 'after'))
# mydfm <- dfm(data_corpus_inaugural, groups = period)
# mydfm <- dfm_select(mydfm, min_count = 2)
# mydfm <- dfm_smooth(mydfm, smoothing = 1)
# textstats_keyness(dfm)

textstat_keyness <- function(x, ...) {
    UseMethod("textstat_keyness")
}

#' @noRd
#' @export
textstat_keyness.dfm <- function(x,  ...) {
    
    if (nrow(x) != 2)
        stop("x must have only two rows")
    
    sums <- rowSums(mx)
    keywords <- apply(x, 2, function(y) keyness(y[1], y[2], sums[1], sums[2]))
    keywords <- sort(keywords, decreasing = TRUE)
    return(keywords)
}

keyness <- function(col, non, sum_col, sum_non){
    tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
    suppressWarnings(
        chi <- stats::chisq.test(tb)
    )
    col_exp <- chi$expected[1,1]
    if(col > col_exp){
        return(unname(chi$statistic))
    }else{
        return(unname(chi$statistic) * -1)
    }
}

