#' Trim a dfm using frequency threshold-based feature selection
#'
#' @description Returns a document by feature matrix reduced in size based on
#'   document and term frequency, usually in terms of a minimum frequency, but
#'   may also be in terms of maximum frequencies.  Setting a combination of
#'   minimum and maximum frequencies will select features based on a range.
#'
#' @description Feature selection is implemented by considering features across
#'   all documents, by summing them for term frequency, or counting the
#'   documents in which they occur for document frequency. Rank and quantile
#'   versions of these are also implemented, for taking the first \eqn{n}
#'   features in terms of descending order of overall global counts or document
#'   frequencies, or as a quantile of all frequencies.
#' @param x a [dfm] object
#' @param min_termfreq,max_termfreq minimum/maximum values of feature frequencies
#' across all documents, below/above which features will
#'   be removed
#' @param termfreq_type how `min_termfreq` and `max_termfreq` are
#'   interpreted.  `"count"` sums the frequencies; `"prop"` divides the
#'   term frequencies by the total sum; `"rank"` is matched against the
#'   inverted ranking of features in terms of overall frequency, so that 1, 2,
#'   ... are the highest and second highest frequency features, and so on;
#'   `"quantile"` sets the cutoffs according to the quantiles (see
#'   [quantile()]) of term frequencies.
#' @param min_docfreq,max_docfreq minimum/maximum values of a feature's document
#'   frequency, below/above which features will be removed
#' @param docfreq_type specify how `min_docfreq` and `max_docfreq` are
#'   interpreted.   `"count"` is the same as `[docfreq](x, scheme
#'   = "count")`; `"prop"` divides the document frequencies by the total
#'   sum; `"rank"` is matched against the inverted ranking of document
#'   frequency, so that 1, 2, ... are the features with the highest and second
#'   highest document frequencies, and so on; `"quantile"` sets the cutoffs
#'   according to the quantiles (see [quantile()]) of document
#'   frequencies.
#' @param sparsity equivalent to `1 - min_docfreq`, included for comparison
#'   with \pkg{tm}
#' @param verbose print messages
#' @param ... not used
#' @return A [dfm] reduced in features (with the same number of documents)
#' @export
#' @note Trimming a [dfm] object is an operation based on the *values*
#'   in the document-feature matrix.  To select subsets of a dfm based on the
#'   features themselves (meaning the feature labels from
#'   [featnames()]) -- such as those matching a regular expression, or
#'   removing features matching a stopword list, use [dfm_select()].
#' @seealso [dfm_select()], [dfm_sample()]
#' @examples
#' (dfmat <- dfm(data_corpus_inaugural[1:5]))
#'
#' # keep only words occurring >= 10 times and in >= 2 documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 2)
#'
#' # keep only words occurring >= 10 times and in at least 0.4 of the documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 0.4)
#'
#' # keep only words occurring <= 10 times and in <=2 documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 2)
#'
#' # keep only words occurring <= 10 times and in at most 3/4 of the documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 0.75)
#'
#' # keep only words occurring 5 times in 1000, and in 2 of 5 of documents
#' dfm_trim(dfmat, min_docfreq = 0.4, min_termfreq = 0.005, termfreq_type = "prop")
#'
#' # keep only words occurring frequently (top 20%) and in <=2 documents
#' dfm_trim(dfmat, min_termfreq = 0.2, max_docfreq = 2, termfreq_type = "quantile")
#'
#' \dontrun{
#' # compare to removeSparseTerms from the tm package
#' (dfmattm <- convert(dfmat, "tm"))
#' tm::removeSparseTerms(dfmattm, 0.7)
#' dfm_trim(dfmat, min_docfreq = 0.3)
#' dfm_trim(dfmat, sparsity = 0.7)
#' }
#'
#' @export
dfm_trim <- function(x,
                     min_termfreq = NULL, max_termfreq = NULL, termfreq_type = c("count", "prop", "rank", "quantile"),
                     min_docfreq = NULL, max_docfreq = NULL, docfreq_type = c("count", "prop", "rank", "quantile"),
                     sparsity = NULL,
                     verbose = quanteda_options("verbose"),
                     ...) {
    UseMethod("dfm_trim")
}

#' @export
dfm_trim.default <- function(x,
                             min_termfreq = NULL, max_termfreq = NULL,
                             termfreq_type = c("count", "prop", "rank", "quantile"),
                             min_docfreq = NULL, max_docfreq = NULL,
                             docfreq_type = c("count", "prop", "rank", "quantile"),
                             sparsity = NULL,
                             verbose = quanteda_options("verbose"),
                             ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_trim"))
}

#' @export
dfm_trim.dfm <- function(x,
                         min_termfreq = NULL, max_termfreq = NULL,
                         termfreq_type = c("count", "prop", "rank", "quantile"),
                         min_docfreq = NULL, max_docfreq = NULL,
                         docfreq_type = c("count", "prop", "rank", "quantile"),
                         sparsity = NULL,
                         verbose = quanteda_options("verbose"),
                         ...) {

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)

    dots <- list(...)
    if ("min_count" %in% names(dots)) {
        warning("min_count is deprecated, use min_termfreq")
        min_termfreq <- dots[["min_count"]]
    }
    if ("max_count" %in% names(dots)) {
        warning("max_count is deprecated, use max_termfreq")
        max_termfreq <- dots[["max_count"]]
    }

    termfreq_type <- match.arg(termfreq_type)
    docfreq_type <- match.arg(docfreq_type)
    attrs <- attributes(x)

    # warning if already fractional
    if ((!is.null(min_termfreq) || !is.null(max_termfreq)) &&
        field_object(attrs, "weight_tf")$scheme != "count" || field_object(attrs, "weight_df")$scheme != "unary") {
        warning("dfm has been previously weighted")
    }
    # warning for fractional term frequency
    if (field_object(attrs, "weight_tf")$scheme == "count" && termfreq_type == "count") {
        if (!is.null(max_termfreq) && max_termfreq < 1)
            warning("use termfreq_type = 'prop' for fractional term frequency")
        if (!is.null(min_termfreq) && min_termfreq < 1)
            warning("use termfreq_type = 'prop' for fractional term frequency")
    }

    freq <- unname(colSums(x))
    freq_doc <- unname(docfreq(x))

    if (!is.null(sparsity)) {
        if (!is.null(max_docfreq) && !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document ",
                 "threshold, both should not be specified")
        if (verbose)
            catm("Note: converting sparsity into min_docfreq = 1 -",
                 sparsity, "=", format(min_docfreq, big.mark = ","), ".\n")
        min_docfreq <- 1.0 - sparsity
        docfreq_type <- "prop"
    }

    s <- sum(freq)
    if (termfreq_type == "count") {
        if (is.null(min_termfreq))
            min_termfreq <- 1
        if (is.null(max_termfreq))
            max_termfreq <- max(freq)
    } else if (termfreq_type == "prop") {
        if (is.null(min_termfreq))
            min_termfreq <- 0
        if (is.null(max_termfreq))
            max_termfreq <- 1
        min_termfreq <- min_termfreq * s
        max_termfreq <- max_termfreq * s
    } else if (termfreq_type ==  "quantile")  {
        if (is.null(min_termfreq))
            min_termfreq <- 0
        if (is.null(max_termfreq))
            max_termfreq <- 1
        min_termfreq <- quantile(freq, min_termfreq, names = FALSE, type = 1)
        max_termfreq <- quantile(freq, max_termfreq, names = FALSE, type = 1)
    } else if (termfreq_type == "rank") {
        if (is.null(min_termfreq))
            min_termfreq <- nfeat(x)
        if (is.null(max_termfreq))
            max_termfreq <- 1
        r <- rank(freq * -1, ties.method = "min")
        min_termfreq <- min(freq[r <= min_termfreq])
        max_termfreq <- max(freq[r >= max_termfreq])
    }

    n <- ndoc(x)
    if (docfreq_type == "count") {
        if (is.null(min_docfreq))
            min_docfreq <- 1
        if (is.null(max_docfreq))
            max_docfreq <- max(freq_doc)
    } else if (docfreq_type == "prop") {
        if (is.null(min_docfreq))
            min_docfreq <- 0
        if (is.null(max_docfreq))
            max_docfreq <- 1
        min_docfreq <- min_docfreq * n
        max_docfreq <- max_docfreq * n
    } else if (docfreq_type ==  "quantile")  {
        if (is.null(min_docfreq))
            min_docfreq <- 0
        if (is.null(max_docfreq))
            max_docfreq <- 1
        min_docfreq <- quantile(freq_doc, min_docfreq, names = FALSE, type = 1)
        max_docfreq <- quantile(freq_doc, max_docfreq, names = FALSE, type = 1)
    } else if (docfreq_type == "rank") {
        if (is.null(min_docfreq))
            min_docfreq <- nfeat(x)
        if (is.null(max_docfreq))
            max_docfreq <- 1
        r <- rank(freq_doc * -1, ties.method = "min")
        min_docfreq <- min(freq_doc[r <= min_docfreq])
        max_docfreq <- max(freq_doc[r >= max_docfreq])
    }

    flag_min_term <- freq < min_termfreq
    flag_max_term <- freq > max_termfreq
    flag_min_doc <- freq_doc < min_docfreq
    flag_max_doc <- freq_doc > max_docfreq
    flag_all <- flag_min_term | flag_max_term | flag_min_doc | flag_max_doc

    # in case no features were removed as a result of filtering conditions
    if (!sum(flag_all)) {
        if (verbose) catm("No features removed.", appendLF = TRUE)
        return(x)
    }

    if (verbose) catm("Removing features occurring: ", appendLF = TRUE)

    # print messages about frequency count removal
    if (verbose && (sum(flag_min_term) || sum(flag_max_term))) {
        if (sum(flag_min_term)) {
            catm("  - fewer than ", min_termfreq, " time",
                 if (min_termfreq != 1L) "s" else "", ": ",
                 format(sum(flag_min_term), big.mark = ","),
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_term)) {
            catm("  - more than ", max_termfreq, " time",
                 if (max_termfreq != 1L) "s" else "", ": ",
                 format(sum(flag_max_term), big.mark = ","),
                 sep = "", appendLF = TRUE)
        }
    }

    # print messages about docfreq removal
    if (verbose && (sum(flag_min_doc) || sum(flag_max_doc))) {
        if (sum(flag_min_doc)) {
            catm("  - in fewer than ", min_docfreq, " document",
                 ifelse(min_docfreq != 1, "s", ""), ": ",
                 format(sum(flag_min_doc), big.mark = ","),
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_doc)) {
            catm("  - in more than ", max_docfreq, " document",
                 ifelse(max_docfreq != 1, "s", ""), ": ",
                 format(sum(flag_max_doc), big.mark = ","),
                 sep = "", appendLF = TRUE)
        }
    }

    if (verbose) {
        catm("  Total features removed: ", format(sum(flag_all), big.mark = ","),
             " (",
             format(sum(flag_all) / nfeat(x) * 100, digits = 3, nsmall = 1),
             "%).",
             sep = "", appendLF = TRUE)
    }

    x[, !flag_all]
}
