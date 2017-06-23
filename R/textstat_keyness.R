#' calculate keyness statistics
#' 
#' @param x a \link{dfm} containing the features to be examined for keyness
#' @param target the document index (numeric, character or logical) identifying the 
#'   document forming the "target" for computing keyness; all other documents' 
#'   feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness.
#'   Currenly available: \code{"chi2"} (\eqn{chi^2} with Yates correction); 
#'   \code{"exact"} (Fisher's exact test); \code{"lr"} for the likelihood ratio
#'   \eqn{G} statistic with Yates correction; \code{"MI"} for the Mutual Information
#'    statistic.
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
#'   
#'   Dunning, Ted. 1993. "Accurate Methods for the Statistics of Surprise and Coincidence", 
#'   \emph{Computational Linguistics}, Vol 19, No. 1, pp. 61-74.
#' @return a data.frame of computed statistics and associated p-values, where the features 
#' scored name each row, and the number of occurrences for both the target and reference groups.  
#'   For \code{measure = "chi2"} this is the chi-squared value, signed 
#'   positively if the observed value in the target exceeds its expected value; 
#'   for \code{measure = "exact"} this is the estimate of the odds ratio; for 
#'   \code{measure = "lr"} this is the likelihood ratio \eqn{G} statistic.
#' @export
#' @keywords textstat
#' @importFrom stats chisq.test
#' @examples
#' # compare pre- v. post-war terms using grouping
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' head(mydfm) # make sure 'post-war' is in the first row
#' head(result <- textstat_keyness(mydfm), 10)
#' tail(result, 10)
#' 
#' # compare pre- v. post-war terms using logical vector
#' mydfm2 <- dfm(data_corpus_inaugural)
#' textstat_keyness(mydfm2, docvars(data_corpus_inaugural, "Year") >= 1945)
#' 
#' # compare Trump 2017 to other post-war preseidents
#' pwdfm <- dfm(corpus_subset(data_corpus_inaugural, period == "post-war"))
#' head(textstat_keyness(pwdfm, target = "2017-Trump"), 10)
#' # using the likelihood ratio method
#' head(textstat_keyness(dfm_smooth(pwdfm), measure = "lr", target = "2017-Trump"), 10)
textstat_keyness <- function(x, target = 1L, measure = c("chi2", "exact", "lr", "MI"), sort = TRUE) {
    UseMethod("textstat_keyness")
}

#' @noRd
#' @export
textstat_keyness.dfm <- function(x, target = 1L, measure = c("chi2", "exact", "lr", "MI"), sort = TRUE) {
    
    # error checking
    measure <- match.arg(measure)
    if (ndoc(x) < 2 )
        stop("x must have at least two documents")
    if (is.character(target) && !(target %in% docnames(x)))
        stop("target not found in docnames(x)")
    if (is.numeric(target) && (target < 1 | target > ndoc(x)))
        stop("target index outside range of documents")
    if (is.logical(target) && length(target) != ndoc(x))
        stop("target must be the same length as the number of documents")
    if (is.logical(target))
        target <- which(target)
    
    # get the target and reference documents by concatenating all non-target docs
    grouping <- rep(2, ndoc(x))
    names(grouping) <- docnames(x)
    grouping[target] <- 1
    rownames(x) <- grouping
    x <- dfm_compress(x, margin = 'documents')
    x <- x[order(docnames(x)), ]

    if (measure == "chi2") {
        keywords <- keyness_chi2_dt(x)
    } else if (measure == "exact") {
        keywords <- keyness_exact(x)
    } else if (measure == "lr") {
        keywords <- keyness_lr(x)
    } else if (measure == "MI") {
        keywords <- keyness_mi(x)
    } else {
        stop(measure, " not yet implemented for textstat_keyness")
    }

    if (sort)
        keywords <- keywords[order(keywords[, 1], decreasing = TRUE), ]
    
    names(keywords)[which(names(keywords) == "target")] <- "n_target"
    names(keywords)[which(names(keywords) == "reference")] <- "n_reference"
    return(keywords)
}


#' compute keyness (internal functions)
#' 
#' Internal function used in textstat_keyness.  Computes \eqn{chi^2} with Yates'
#' continuity correction for 2x2 tables.
#' @name keyness
#' @param x a \link{dfm} object
#' @details \code{keyness_chi2_dt} uses vectorized computation from data.table
#' objects.
#' @return a data.frame of chi2 and p-values with rows named for each feature
#' @examples
#' mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
#'                d2 = "a a b c c d d d d e f h"))
#' quanteda:::keyness_chi2_dt(mydfm)
#' @keywords textstat internal
#' @import data.table
#' @importFrom stats dchisq
#' @references
#'   \url{https://en.wikipedia.org/wiki/Yates's_correction_for_continuity}
#'   
#'   
keyness_chi2_dt <- function(x) {
    
    a <- b <- c <- d <- N <- E <- chi2 <- p <- NULL 
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a+b+c+d)]
    dt[, E := (a+b)*(a+c) / N]
    # compute using the direct formula - see link above (adds sign)
    dt[, chi2 := (N * (abs(a*d - b*c) - N/2)^2) / ((a+b)*(c+d)*(a+c)*(b+d)) * 
                 ifelse(a > E, 1, -1)]

    # compute p-values
    dt[, p := 1 - stats::pchisq(abs(chi2), 1)]

    result <- as.data.frame(dt[, list(chi2, p)])
    rownames(result) <- dt$feature
    result$target = as.vector(x[1,])
    result$reference = as.vector(x[2,])
    return(result)
}
    
#' @rdname keyness
#' @importFrom stats chisq.test
#' @details 
#' \code{keyness_chi2_stats} uses element-by-element application of
#' \link[stats]{chisq.test}.
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
        list(chi2 = unname(chi$statistic) * ifelse(t > t_exp, 1, -1),
             p = unname(chi$p.value))
    }
    sums <- rowSums(x)
    result <- as.data.frame(
        do.call(rbind, apply(x, 2, function(y) keyness(as.numeric(y[1]), 
                                                       as.numeric(y[2]), 
                                                       sums[1], sums[2])))
    )
    result$target = as.vector(x[1,])
    result$reference = as.vector(x[2,])
    return(result)
}


#' @rdname keyness
#' @details 
#' \code{keyness_exact} computes Fisher's exact using element-by-element 
#' application of \link[stats]{fisher.test}, returning the odds ratio.
#' @importFrom stats fisher.test
#' @examples
#' quanteda:::keyness_exact(mydfm)
keyness_exact <- function(x) {
    sums <- rowSums(x)
    result <- as.data.frame(
        do.call(rbind, 
                apply(x, 2, function(y) { 
                    et <- stats::fisher.test(matrix(c(as.numeric(y), as.numeric(sums - y)), nrow = 2))
                    data.frame(or = as.numeric(et$estimate), p = et$p.value)
                }))
    )
    result$target = as.vector(x[1,])
    result$reference = as.vector(x[2,])
    return(result)
}


#' @rdname keyness
#' @param correction if \code{"Yates"} implement the Yates correction for 2x2 
#'   tables, no correction if \code{"none"}
#' @details \code{keyness_lr} computes the \eqn{G^2} likelihood ratio statistic
#'   using vectorized computation
#' @examples
#' quanteda:::keyness_lr(mydfm)
#' @references
#' \url{http://influentialpoints.com/Training/g-likelihood_ratio_test.htm}
keyness_lr <- function(x, correction = c("none", "Yates")) {
    
    correction <- match.arg(correction)
    
    a <- b <- c <- d <- N <- E11 <- G <- p <- NULL 
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E11 := (a+b)*(a+c) / N]
    
    if (correction == "Yates") {
        # implement Yates continuity correction
        # If (ad-bc) is positive, subtract 0.5 from a and d and add 0.5 to b and c. 
        # If (ad-bc) is negative, add 0.5 to a and d and subtract 0.5 from b and c.
        dt[, correction := a*d - b*c > 0]
        dt[, c("a", "d", "b", "c") := list(a + ifelse(correction, -0.5, 0.5),
                                           d + ifelse(correction, -0.5, 0.5),
                                           b + ifelse(correction, 0.5, -0.5),
                                           c + ifelse(correction, 0.5, -0.5))]
    } 
    ## the other possible correction to implement is the Williams correction, 
    ## see http://influentialpoints.com/Training/g-likelihood_ratio_test.htm
    
    dt[, G := (2 * (a * log(a / E11) + 
                     b * log(b / ((a+b)*(b+d) / N)) +
                     c * log(c / ((a+c)*(c+d) / N)) +
                     d * log(d / ((b+d)*(c+d) / N)))) *
               ifelse(a > E11, 1, -1)]
    
    # compute p-values
    dt[, p := 1 - stats::pchisq(abs(G), 1)]
    
    result <- as.data.frame(dt[, list(G, p)])
    rownames(result) <- dt$feature
    result$target = as.vector(x[1,])
    result$reference = as.vector(x[2,])
    return(result)
}

#' @rdname keyness
#' @details \code{keyness_mi} computes the Mutual Information statistic
#'   using vectorized computation
#' @examples
#' quanteda:::keyness_mi(mydfm)
#' @references
keyness_mi <- function(x) {
    
    a <- b <- c <- d <- N <- E11 <- MI <- p <- NULL 
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E11 := (a+b)*(a+c) / N]
    
    dt[, MI := (  (a * log(a  / E11) / N+ 
                        b * log(b * N/ ((a+b)*(b+d) )) / N +
                        c * log(c * N/ ((a+c)*(c+d) )) / N +
                        d * log(d * N/ ((b+d)*(c+d) )) / N)) ]
    
    # compute p-values
    dt[, p := stats::pchisq(abs(MI), 1)]
    
    result <- as.data.frame(dt[, list(MI, p)])
    rownames(result) <- dt$feature
    result$target = as.vector(x[1,])
    result$reference = as.vector(x[2,])
    return(result)
}


