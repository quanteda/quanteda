#' calculate keyness statistics
#' 
#' @param x a \link{dfm} containing the features to be examined for keyness
#' @param target the document index (numeric or character) identifying the 
#'   document forming the "target" for computing keyness; all other documents'
#'   feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness. 
#'   Currenly available: \code{"chi2"} (chi-squared with Yates correction).
#' @param sort logical; if \code{TRUE} sort features scored in descending order 
#'   of the measure, otherwise leave in original feature order
#' @references Bondi, Marina, and Mike Scott, eds. 2010.  \emph{Keyness in
#'   Texts}. Amsterdam, Philadelphia: John Benjamins, 2010.
#'   
#'   Stubbs, Michael. 2010.  "Three Concepts of Keywords". In \emph{Keyness in
#'   Texts}, Marina Bondi and Mike Scott, eds. pp21–42. Amsterdam, Philadelphia:
#'   John Benjamins.
#'   
#'   Scott, M. & Tribble, C. 2006.  \emph{Textual Patterns: keyword and corpus
#'   analysis in language education}.  Amsterdam: Benjamins, p. 55.
#' @return a named numeric vector of keyness scores, named with the features
#' @export
#' @keywords textstat experimental
#' @importFrom stats chisq.test
#' @examples
#' # compare pre- v. post-war terms using grouping
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' head(mydfm) # make sure 'post-war' is in the first row
#' head(result <- textstat_keyness(mydfm), 10)
#' tail(result, 10)
#' 
#' # compare Trump 2017 to other post-war preseidents
#' pwdfm <- dfm(corpus_subset(data_corpus_inaugural, period == "post-war"))
#' head(textstat_keyness(pwdfm, target = "2017-Trump"), 10)
textstat_keyness <- function(x, target = 1L, measure = "chi2", sort = TRUE) {
    UseMethod("textstat_keyness")
}

#' @noRd
#' @export
textstat_keyness.dfm <- function(x, target = 1L, measure = "chi2", sort = TRUE) {
    
    # error checking
    measure <- match.arg(measure)
    if (ndoc(x) < 2 )
        stop("x must have at least two documents")
    if (is.character(target) && !(target %in% docnames(x)))
        stop("target not found in docnames(x)")
    if (is.numeric(target) && (target < 1 | target > ndoc(x)))
        stop("target index outside range of documents")
    
    # get the target and reference documents by concatenating all non-target docs
    grouping <- rep(2, ndoc(x))
    names(grouping) <- docnames(x)
    grouping[target] <- 1
    x <- dfm(x, groups = grouping)
    x <- x[order(docnames(x)), ]

    if (measure == "chi2") {
        keywords <- keyness_chi2_dt(x)
    } else {
        stop(measure, " not yet implemented for textstat_keyness")
    }

    if (sort)
        keywords <- sort(keywords, decreasing = TRUE)
    
    return(keywords)
}


#' compute keyness using chi2 statistic
#' 
#' Internal function used in textstat_keyness.  Computes Chi^2 with Yates'
#' continuity correction for 2x2 tables.
#' @name keyness_chi2
#' @details \code{keyness_chi2_dt} uses vectorized computation from data.table
#' objects.
#' 
#' \code{keyness_chi2_stats} uses element-by-element application of
#' \link[stats]{chisq.test}.
#' @return named numeric vector containing the chi2 value for each feature
#' @examples
#' mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
#'                d2 = "a a b c c d d d d e f h"))
#' quanteda:::keyness_chi2_dt(mydfm)
#' @keywords textstat internal
#' @import data.table
#' @references
#'   \url{https://en.wikipedia.org/wiki/Yates's_correction_for_continuity}
keyness_chi2_dt <- function(x) {
    a <- b <- c <- d <- N <- E <- chi2 <- NULL 
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E := (a+b)*(a+c) / N]
    # compute using the direct formula - see link above (adds sign)
    dt[, chi2 := (N * (abs(a*d - b*c) - N/2)^2) / ((a+b)*(c+d)*(a+c)*(b+d)) * 
                 ifelse(a > E, 1, -1)]

    result <- dt$chi2
    names(result) <- dt$feature
    result
}
    
#' @rdname keyness_chi2
#' @examples 
#' quanteda:::keyness_chi2_stats(mydfm)
keyness_chi2_stats <- function(x) {
    keyness <- function(t, f, sum_t, sum_f) {
        # @param t (scalar) frequency of target 
        # @param f (scalar) frequency of reference
        # @param sum_t total of all target words
        # @param sum_f total of all reference words
        tb <- as.table(rbind(c(t, f), c(sum_t - t, sum_f - f)))
        suppressWarnings(chi <- stats::chisq.test(tb))
        t_exp <- chi$expected[1,1]
        unname(chi$statistic) * ifelse(t > t_exp, 1, -1)
    }
    sums <- rowSums(x)
    apply(x, 2, function(y) keyness(as.numeric(y[1]), 
                                    as.numeric(y[2]), 
                                    sums[1], sums[2]))
}

