# dfm_goodturing ---------------

#' Simple Good-Turing Frequency Estimation
#'
#' @param x object for which frequency estimates will be computed (a
#'   document-feature matrix)
#' @param scheme scheme for output; The default is `"est_count"`, the estimated
#'   frequency that would have been observed if the sample were perfectly
#'   representative of the population (sometimes known as r*). `"est_prop"`
#'   gives the corresponding estimate of the relative frequency, correcting for
#'   unobserved features in the sample.
#' @inheritParams dfm_weight
#' @param ... additional arguments passed to \code{\link{docfreq}}.
#' @details `dfm_goodturing` computes estimated term frequencies, correcting for
#'   unobserved cases in the sample. This method was pioneered by Alan Turing
#'   and I. J. Good for cracking German ciphers during World War II. The
#'   particular algorithm implemented here was described by Gale & Sampson
#'   (1995). Without this correction, observed token frequencies will
#'   systematically overestimate population frequencies, since there are many
#'   infrequent tokens that have not yet been observed in the sample.
#' @references Gale, W. A., & Sampson, G. (1995).
#'   Good-Turing frequency estimation without tears.
#'   *J. Quant. Linguistics*, 2, 217-237.
#'   <https://doi.org/10.1080/09296179508590051>
#' @keywords dfm weighting
#' @examples
#' dfmat1 <- dfm(tokens(data_corpus_inaugural))
#' head(dfmat1[, 5:10])
#' head(dfm_goodturing(dfmat1)[, 5:10])
#' @keywords dfm weighting
#' @export
dfm_goodturing <- function(x, scheme = "est_count") {
    UseMethod("dfm_goodturing")
}

#' @export
dfm_goodturing.default <- function(x, scheme = "est_count") {
    check_class(class(x), "dfm_goodturing")
}

#' @export
dfm_goodturing.dfm <- function(x, scheme = "est_count") {
    # variable names follow Gale & Sampson (1995)

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) {
        return(x)
    }

    # tabulate
    # docs - rows
    # species (r) - colnames
    # distinct species occurring with each species frequency (n) - values
    tab <- table(rep(rownames(x), ncol(x)), as.matrix(x))
    if (colnames(tab)[1] == "0") {
        tab <- tab[, -1] # remove zero column
    }

    # named vector of sample sizes
    N <- ntoken(x)

    # relative frequency of hapax legomena
    P0 <- tab[, 1] / N

    r <- as.integer(colnames(tab)) # r (see above)
    logr_logZ_list <- lapply(
        1:ndoc(x),
        function(d) {
            # remove zero values for doc
            nonzero_cols <- which(tab[d, ] != 0)
            r_doc <- r[nonzero_cols]
            n_doc <- tab[d, nonzero_cols]

            i <- c(0, head(r_doc, -1)) # lag
            last_k <- 2 * tail(r_doc, 1L) - tail(i, 1L)
            k <- c(tail(r_doc, -1), last_k) # lead

            Z <- 2 * n_doc / (k - i)

            logr_logZ <- data.frame(
                logr = log(r_doc),
                logr1 = log(r_doc + 1),
                logZ = log(Z)
            )

            lmfit <- lm(logZ ~ logr, data = logr_logZ)

            logr_logZ_new <- data.frame(logr = logr_logZ$logr1)

            r_star <- rep(NA, length(r_doc))
            r_star_x <- rep(NA, length(r_doc))
            r_star_y <- (r_doc + 1) * exp(predict(lmfit, newdata = logr_logZ_new)) / exp(predict(lmfit))

            crit_passed <- FALSE
            for (j in 1:length(n_doc)) {
                # check for gap in n_doc
                if (!crit_passed) {
                    if (is.na(n_doc[as.character(r_doc[j] + 1)])) {
                        crit_passed <- TRUE
                    }
                }
                # computation
                if (!crit_passed) {
                    r_star_x[j] <- (r_doc[j] + 1) * n_doc[j + 1] / n_doc[j]
                    crit <- abs(r_star_x[j] - r_star_y[j]) > 1.96 * sqrt((1 + n_doc[j + 1] / n_doc[j]) * ((r_doc[j] + 1)^2) * (n_doc[j + 1] / n_doc[j]^2))
                    r_star[j] <- ifelse(crit, r_star_x[j], r_star_y[j])
                } else {
                    r_star[j] <- r_star_y[j]
                }
            }

            r_star_doc <- tab[d, ]
            r_star_doc[nonzero_cols] <- r_star

            if (scheme == "est_count") {
                return(r_star_doc)
            } else if (scheme == "est_prop") {
                return((1 - P0[d]) * r_star_doc / sum(tab[d, ] * r_star_doc))
            }
        }
    )

    # retrieve value estimates
    x@x <- unlist(
        lapply(1:length(x@i), function(row) logr_logZ_list[[x@i[row] + 1L]][as.character(x@x[row])])
    )

    attrs <- attributes(x)
    field_object(attrs, "weight_df") <- c(
        list(scheme = scheme)
    )
    rebuild_dfm(x, attrs)
}
