#' Calculate keyness statistics
#'
#' Calculate "keyness", a score for features that occur differentially across
#' different categories.  Here, the categories are defined by reference to a
#' "target" document index in the [dfm], with the reference group
#' consisting of all other documents.
#' @param x a [dfm] containing the features to be examined for keyness
#' @param target the document index (numeric, character or logical) identifying
#'   the document forming the "target" for computing keyness; all other
#'   documents' feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness.
#'   Currently available: `"chi2"`; `"exact"` (Fisher's exact test); `"lr"` for
#'   the likelihood ratio; `"pmi"` for pointwise mutual information.  Note that
#'   the "exact" test is very computationally intensive and therefore much
#'   slower than the other methods.
#' @param sort logical; if `TRUE` sort features scored in descending order
#'   of the measure, otherwise leave in original feature order
#' @param correction if `"default"`, Yates correction is applied to
#'   `"chi2"`; William's correction is applied to `"lr"`; and no
#'   correction is applied for the `"exact"` and `"pmi"` measures.
#'   Specifying a value other than the default can be used to override the
#'   defaults, for instance to apply the Williams correction to the chi2
#'   measure.  Specifying a correction for the `"exact"` and `"pmi"`
#'   measures has no effect and produces a warning.
#' @param ... not used
#' @references Bondi, M. & Scott, M. (eds) (2010). *Keyness in
#'   Texts*. Amsterdam, Philadelphia: John Benjamins.
#'
#'   Stubbs, M. (2010). Three Concepts of Keywords. In *Keyness in
#'   Texts*, Bondi, M. & Scott, M. (eds): 1--42. Amsterdam, Philadelphia:
#'   John Benjamins.
#'
#'   Scott, M. & Tribble, C. (2006). *Textual Patterns: Keyword and Corpus
#'   Analysis in Language Education*. Amsterdam: Benjamins: 55.
#'
#'   Dunning, T. (1993). [Accurate Methods for the Statistics of Surprise and
#'   Coincidence](https://dl.acm.org/doi/10.5555/972450.972454). *Computational
#'   Linguistics*, 19(1): 61--74.
#' @return a data.frame of computed statistics and associated p-values, where
#'   the features scored name each row, and the number of occurrences for both
#'   the target and reference groups. For `measure = "chi2"` this is the
#'   chi-squared value, signed positively if the observed value in the target
#'   exceeds its expected value; for `measure = "exact"` this is the
#'   estimate of the odds ratio; for `measure = "lr"` this is the
#'   likelihood ratio \eqn{G2} statistic; for `"pmi"` this is the pointwise
#'   mutual information statistics.
#' @export
#' @return `textstat_keyness` returns a data.frame of features and
#'   their keyness scores and frequency counts.
#' @keywords textstat
#' @importFrom stats chisq.test
#' @examples
#' # compare pre- v. post-war terms using grouping
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' dfmat1 <- dfm(data_corpus_inaugural, groups = period)
#' head(dfmat1) # make sure 'post-war' is in the first row
#' head(tstat1 <- textstat_keyness(dfmat1), 10)
#' tail(tstat1, 10)
#'
#' # compare pre- v. post-war terms using logical vector
#' dfmat2 <- dfm(data_corpus_inaugural)
#' head(textstat_keyness(dfmat2, docvars(data_corpus_inaugural, "Year") >= 1945), 10)
#'
#' # compare Trump 2017 to other post-war preseidents
#' dfmat3 <- dfm(corpus_subset(data_corpus_inaugural, period == "post-war"))
#' head(textstat_keyness(dfmat3, target = "2017-Trump"), 10)
#'
#' # using the likelihood ratio method
#' head(textstat_keyness(dfm_smooth(dfmat3), measure = "lr", target = "2017-Trump"), 10)
textstat_keyness <- function(x, target = 1L,
                             measure = c("chi2", "exact", "lr", "pmi"),
                             sort = TRUE,
                             correction = c("default", "yates", "williams", "none"),
                             ...) {
    UseMethod("textstat_keyness")
}

#' @export
textstat_keyness.default <- function(x, target = 1L,
                                     measure = c("chi2", "exact", "lr", "pmi"),
                                     sort = TRUE,
                                     correction = c("default", "yates", "williams", "none"),
                                     ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_keyness"))
}

#' @export
textstat_keyness.dfm <- function(x, target = 1L, measure = c("chi2", "exact", "lr", "pmi"),
                                 sort = TRUE,
                                 correction = c("default", "yates", "williams", "none"),
                                 ..., old = FALSE) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))

    measure <- match.arg(measure)
    correction <- match.arg(correction)
    # error checking
    if (ndoc(x) < 2)
        stop("x must have at least two documents")
    if (is.character(target) && !(all(target %in% docnames(x))))
        stop("target not found in docnames(x)")
    if (is.numeric(target) && any(target < 1 | target > ndoc(x)))
        stop("target index outside range of documents")
    if (is.logical(target) && length(target) != ndoc(x))
        stop("logical target value length must equal the number of documents")

    # convert all inputs into logical vector
    if (is.numeric(target)) {
        target <- seq(ndoc(x)) %in% target
    } else if (is.character(target)) {
        target <- docnames(x) %in% target
    } else if (is.logical(target)) {
        target <- target
    } else {
        stop("target must be numeric, character or logical")
    }

    # check if number of target documents < ndoc
    if (sum(target) >= ndoc(x)) {
        stop("target cannot be all the documents")
    }

    # use original docnames only when there are two (different) documents
    if (ndoc(x) == 2 && length(unique(docnames(x))) > 1) {
        label <- docnames(x)[order(target, decreasing = TRUE)]
    } else {
        if (sum(target) == 1 && !is.null(docnames(x)[target])) {
            label <- c(docnames(x)[target], "reference")
        } else {
            label <- c("target", "reference")
        }
    }
    grouping <- factor(target, levels = c(TRUE, FALSE), labels = label)
    temp <- dfm_group(x, groups = grouping)

    if (old) {
        if (measure == "chi2") {
            result <- keyness_chi2_dt(temp, correction)
        } else if (measure == "lr") {
            result <- keyness_lr(temp, correction)
        } else if (measure == "exact") {
            if (!correction %in% c("default", "none"))
                warning("correction is always none for exact")
            result <- keyness_exact(temp)
        } else if (measure == "pmi") {
            if (!correction %in% c("default", "none"))
                warning("correction is always none for pmi")
            result <- keyness_pmi(temp)
        }
    } else {
        if (measure == "exact") {
            if (measure == "exact" && !correction %in% c("default", "none"))
                warning("correction is always none for exact")
            result <- keyness_exact(temp)
        } else {
            if (measure == "pmi" && !correction %in% c("default", "none"))
                warning("correction is always none for pmi")
            result <- data.frame(
                feature = featnames(temp),
                stat = qatd_cpp_keyness(temp, measure, correction),
                p = NA,
                n_target = as.vector(temp[1, ]),
                n_reference = as.vector(temp[2, ]),
                stringsAsFactors = FALSE
            )
            result$p <- 1 - stats::pchisq(abs(result$stat), 1) # abs() for pmi
        }
    }
    if (sort)
        result <- result[order(result$stat, result$feature,
                               decreasing = c(TRUE, FALSE), method = "radix"), ]
    names(result)[2] <- switch(measure, chi2 = "chi2", exact = "exact", lr = "G2", pmi = "pmi")
    rownames(result) <- NULL
    attr(result, "groups") <- docnames(temp)
    class(result) <- c("keyness", "textstat", "data.frame")
    return(result)
}

#' Compute keyness (internal functions)
#'
#' Internal function used in textstat_keyness.  Computes \eqn{chi^2} with Yates'
#' continuity correction for 2x2 tables.
#' @name keyness
#' @param x a [dfm] object
#' @details `keyness_chi2_dt` uses vectorized computation from data.table
#' objects.
#' @return a data.frame of chi2 and p-values with rows named for each feature
#' @examples
#' dfmat <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
#'                d2 = "a a b c c d d d d e f h"))
#' quanteda:::keyness_chi2_dt(dfmat)
#' @keywords textstat internal
#' @importFrom data.table data.table :=
#' @importFrom stats dchisq
#' @references
#'   <https://en.wikipedia.org/wiki/Yates's_correction_for_continuity>
#'
#'
keyness_chi2_dt <- function(x, correction = c("default", "yates", "williams", "none")) {

    correction <- match.arg(correction)
    a <- b <- c <- d <- N <- E <- chi2 <- p <- cor_app <- q <- NULL
    if (ndoc(x) > 2) stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E := (a + b) * (a + c) / N]

    if (correction == "default" | correction == "yates") {
        dt[, cor_app := (((a + b) * (a + c) / N < 5 | (a + b) * (b + d) / N < 5 |
                              (a + c) * (c + d) / N < 5 | (c + d) * (b + d) / N < 5)
                         & abs(a * d - b * c) >= N / 2)]
        # the correction is usually only recommended if the smallest expected frequency is less than 5
        # the correction should not be applied if |ad - bc| is less than N/2.
        # compute using the direct formula - see link above (adds sign)
        dt[, chi2 := ifelse(cor_app,
                            (N * (abs(a * d - b * c) - N * 0.5) ^ 2) /
                                ((a + b) * (c + d) * (a + c) * (b + d)) * ifelse(a > E, 1, -1),
                            (N * abs(a * d - b * c) ^ 2) /
                                ((a + b) * (c + d) * (a + c) * (b + d)) * ifelse(a > E, 1, -1))]
    } else if (correction == "williams") {
        # William's correction cannot be used if there are any zeros in the table
        # \url{http://influentialpoints.com/Training/g-likelihood_ratio_test.htm}
        dt[, q := ifelse(a * b * c * d == 0, 1,
                         1 + (N / (a + b) + N / (c + d) - 1) * (N / (a + c) + N / (b + d) - 1) / (6 * N))]
        dt[, chi2 := (N * abs(a * d - b * c) ^ 2) / ((a + b) * (c + d) * (a + c) * (b + d)) * ifelse(a > E, 1, -1) / q]
    } else {
        dt[, chi2 := (N * abs(a * d - b * c) ^ 2) / ((a + b) * (c + d) * (a + c) * (b + d)) * ifelse(a > E, 1, -1)]
    }

    # compute p-values
    dt[, p := 1 - stats::pchisq(abs(chi2), 1)]

    data.frame(feature = dt$feature,
               chi2 = dt[, chi2],
               p = dt[, p],
               n_target = as.vector(x[1, ]),
               n_reference = as.vector(x[2, ]),
               stringsAsFactors = FALSE)
}

#' @rdname keyness
#' @importFrom stats chisq.test
#' @importFrom data.table data.table :=
#' @details
#' `keyness_chi2_stats` uses element-by-element application of
#' [chisq.test][stats::chisq.test].
#' @examples
#' quanteda:::keyness_chi2_stats(dfmat)
keyness_chi2_stats <- function(x) {

    sums <- rowSums(x)
    result <- as.data.frame(
        do.call(rbind, apply(x, 2, function(y) keyness(as.numeric(y[1]),
                                                              as.numeric(y[2]),
                                                              sums[1], sums[2])))
    )

    data.frame(feature = colnames(x),
               chi2 = result$chi2,
               p = result$p,
               n_target = as.vector(x[1, ]),
               n_reference = as.vector(x[2, ]),
               stringsAsFactors = FALSE)
}

#' @rdname keyness
#' @param t (scalar) frequency of target
#' @param f (scalar) frequency of reference
#' @param sum_t total of all target words
#' @param sum_f total of all reference words
#' @keywords internal
keyness <- function(t, f, sum_t, sum_f) {

    tb <- as.table(rbind(c(t, f), c(sum_t - t, sum_f - f)))
    suppressWarnings(chi <- stats::chisq.test(tb))
    t_exp <- chi$expected[1, 1]
    list(chi2 = unname(chi$statistic) * ifelse(t > t_exp, 1, -1),
         p = unname(chi$p.value))
}

#' @rdname keyness
#' @details
#' `keyness_exact` computes Fisher's exact using element-by-element
#' application of [fisher.test][stats::fisher.test], returning the odds ratio.
#' @importFrom stats fisher.test
#' @examples
#' quanteda:::keyness_exact(dfmat)
keyness_exact <- function(x) {
    sums <- rowSums(x)
    temp <- as.data.frame(
        do.call(rbind,
                apply(x, 2, function(y) {
                    et <- stats::fisher.test(matrix(c(as.numeric(y),
                                                      as.numeric(sums - y)),
                                                    nrow = 2))
                    data.frame(or = as.numeric(et$estimate), p = et$p.value)
                }))
    )

    data.frame(feature = colnames(x),
               stat = temp$or,
               p = temp$p,
               n_target = as.vector(x[1, ]),
               n_reference = as.vector(x[2, ]),
               stringsAsFactors = FALSE)
}

#' @rdname keyness
#' @param correction implement the Yates correction for 2x2 tables
#' @details `keyness_lr` computes the \eqn{G^2} likelihood ratio statistic
#'   using vectorized computation
#' @examples
#' quanteda:::keyness_lr(dfmat)
#' @references
#' <https://influentialpoints.com/Training/g-likelihood_ratio_test.htm>
keyness_lr <- function(x, correction = c("default", "yates", "williams", "none")) {

    correction <- match.arg(correction)
    epsilon <- 0.000000001; # to offset zero cell counts
    a <- b <- c <- d <- N <- E11 <- G2 <- p <- cor_app <- correction_sign <- q <- NULL
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E11 := (a + b) * (a + c) / N]

    if (correction == "default" | correction == "yates") {

        dt[, cor_app := (((a + b) * (a + c) / N < 5 | (a + b) * (b + d) / N < 5 |
                              (a + c) * (c + d) / N < 5 | (c + d) * (b + d) / N < 5)
                         & abs(a * d - b * c) > N / 2)]
        # the correction is usually only recommended if the smallest expected frequency is less than 5
        # the correction should not be applied if |ad - bc| is less than N/2.
        # implement Yates continuity correction
        # If (ad-bc) is positive, subtract 0.5 from a and d and add 0.5 to b and c.
        # If (ad-bc) is negative, add 0.5 to a and d and subtract 0.5 from b and c.
        dt[, correction_sign := a * d - b * c > 0]
        dt[, c("a", "d", "b", "c") := list(a + ifelse(cor_app, ifelse(correction_sign, -0.5, 0.5), 0),
                                           d + ifelse(cor_app, ifelse(correction_sign, -0.5, 0.5), 0),
                                           b + ifelse(cor_app, ifelse(correction_sign, 0.5, -0.5), 0),
                                           c + ifelse(cor_app, ifelse(correction_sign, 0.5, -0.5), 0))]
    }

    dt[, G2 := (2 * (a * log(a / E11 + epsilon) +
                         b * log(b / ((a + b) * (b + d) / N) + epsilon) +
                         c * log(c / ((a + c) * (c + d) / N) + epsilon) +
                         d * log(d / ((b + d) * (c + d) / N) + epsilon))) *
           ifelse(a > E11, 1, -1)]

    if (correction == "williams") {
        # William's correction cannot be used if there are any zeros in the table
        # \url{http://influentialpoints.com/Training/g-likelihood_ratio_test.htm}
        dt[, q := ifelse(a * b * c * d == 0,
                         1,
                         1 + (N / (a + b) + N / (c + d) - 1) * (N / (a + c) + N / (b + d) - 1) / (6 * N))]
        dt[, G2 := G2 / q]
    }

    # compute p-values
    dt[, p := 1 - stats::pchisq(abs(G2), 1)]

    data.frame(feature = dt$feature,
               G2 = dt[, G2],
               p = dt[, p],
               n_target = as.vector(x[1, ]),
               n_reference = as.vector(x[2, ]),
               stringsAsFactors = FALSE)
}

#' @rdname keyness
#' @details `keyness_pmi` computes the Pointwise Mutual Information stat
#'   using vectorized computation
#' @examples
#' quanteda:::keyness_pmi(dfmat)
keyness_pmi <- function(x) {

    a <- b <- c <- d <- N <- E11 <- pmi <- p <- NULL
    if (ndoc(x) > 2)
        stop("x can only have 2 rows")
    dt <- data.table(feature = featnames(x),
                     a = as.numeric(x[1, ]),
                     b = as.numeric(x[2, ]))
    dt[, c("c", "d") := list(sum(x[1, ]) - a, sum(x[2, ]) - b)]
    dt[, N := (a + b + c + d)]
    dt[, E11 := (a + b) * (a + c) / N]
    epsilon <- .000000001  # to offset zero cell counts
    dt[, pmi :=   log(a / E11 + epsilon)]

    # normalized pmi
    # dt[, pmi :=   log(a  / E11) * ifelse(a > E11, 1, -1)/(-log(a/N)) ]

    # compute p-values
    dt[, p := 1 - stats::pchisq(abs(pmi), 1)]

    data.frame(feature = dt$feature,
               pmi = dt[, pmi],
               p = dt[, p],
               n_target = as.vector(x[1, ]),
               n_reference = as.vector(x[2, ]),
               stringsAsFactors = FALSE)
}
