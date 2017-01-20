#' calculate keyness statistics
#' 
#' @param x a dfm in which the first row is the target and the second is the reference document.
#' @param ... not used
#' @references Bondi, Marina, and Mike Scott, eds. Keyness in Texts. Amsterdam, 
#'   Philadelphia: John Benjamins, 2010.
#'   Stubbs, Michael. ‘Three Concepts of Keywords’. In Keyness in Texts, edited 
#'   by Marina Bondi and Mike Scott, 21–42. Amsterdam, Philadelphia: John Benjamins, 2010.
#' @return a named vector of keyness scores
#' @export
#' @examples
#' period <- as.factor(ifelse(docvars(data_corpus_inaugural)$Year < 1945, 'post-war', 'pre-war'))
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' mydfm <- dfm_remove(mydfm, stopwords('english'))
#' mydfm <- dfm_select(mydfm, min_count = 2, min_nchar = 2)
#' mydfm <- dfm_smooth(mydfm, smoothing = 1)
#' rownames(mydfm) # make sure 'post-war' is in the first row
#' head(textstat_keyness(mydfm), 20)

textstat_keyness <- function(x, ...) {
    UseMethod("textstat_keyness")
}

#' @noRd
#' @export
textstat_keyness.dfm <- function(x,  ...) {
    
    if (nrow(x) != 2)
        stop("x must have only two rows")
    sums <- rowSums(x)
    keywords <- apply(x, 2, function(y) keyness(y[1], y[2], sums[1], sums[2]))
    keywords <- sort(keywords, decreasing = TRUE)
    return(keywords)
}

keyness <- function(t, f, sum_t, sum_f){
    tb <- as.table(rbind(c(t, f), c(sum_t - t, sum_f - f)))
    suppressWarnings(
        chi <- stats::chisq.test(tb)
    )
    t_exp <- chi$expected[1,1]
    if(t > t_exp){
        return(unname(chi$statistic))
    }else{
        return(unname(chi$statistic) * -1)
    }
}

