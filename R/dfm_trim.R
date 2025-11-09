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
#' @inheritParams messages
#' @return A [dfm] reduced in features (with the same number of documents)
#' @export
#' @note Trimming a [dfm] object is an operation based on the *values*
#'   in the document-feature matrix.  To select subsets of a dfm based on the
#'   features themselves (meaning the feature labels from
#'   [featnames()]) -- such as those matching a regular expression, or
#'   removing features matching a stopword list, use [dfm_select()].
#' @seealso [dfm_select()], [dfm_sample()]
#' @examples
#' dfmat <- dfm(tokens(data_corpus_inaugural))
#'
#' # keep only words occurring >= 10 times and in >= 2 documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 2)
#'
#' # keep only words occurring >= 10 times and in at least 0.4 of the documents
#' dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 0.4, docfreq_type = "prop")
#'
#' # keep only words occurring <= 10 times and in <=2 documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 2)
#'
#' # keep only words occurring <= 10 times and in at most 3/4 of the documents
#' dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 0.75, docfreq_type = "prop")
#'
#' # keep only words occurring 5 times in 1000, and in 2 of 5 of documents
#' dfm_trim(dfmat, min_docfreq = 0.4, min_termfreq = 0.005, termfreq_type = "prop")
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
                     min_termfreq = NULL, max_termfreq = NULL, 
                     termfreq_type = c("count", "prop", "rank", "quantile"),
                     min_docfreq = NULL, max_docfreq = NULL, 
                     docfreq_type = c("count", "prop", "rank", "quantile"),
                     sparsity = NULL,
                     verbose = quanteda_options("verbose")) {
    UseMethod("dfm_trim")
}

#' @export
dfm_trim.default <- function(x,
                             min_termfreq = NULL, max_termfreq = NULL,
                             termfreq_type = c("count", "prop", "rank", "quantile"),
                             min_docfreq = NULL, max_docfreq = NULL,
                             docfreq_type = c("count", "prop", "rank", "quantile"),
                             sparsity = NULL,
                             verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_trim")
}

#' @export
dfm_trim.dfm <- function(x,
                         min_termfreq = NULL, max_termfreq = NULL,
                         termfreq_type = c("count", "prop", "rank", "quantile"),
                         min_docfreq = NULL, max_docfreq = NULL,
                         docfreq_type = c("count", "prop", "rank", "quantile"),
                         sparsity = NULL,
                         verbose = quanteda_options("verbose")) {

    x <- as.dfm(x)
    termfreq_type <- match.arg(termfreq_type)
    docfreq_type <- match.arg(docfreq_type)
    verbose <- check_logical(verbose)
    attrs <- attributes(x)
    
    if (!nfeat(x) || !ndoc(x)) return(x)

    # warning if already fractional
    if ((!is.null(min_termfreq) || !is.null(max_termfreq)) &&
        field_object(attrs, "weight_tf")$scheme != "count" || 
        field_object(attrs, "weight_df")$scheme != "unary") {
        warning("dfm has been previously weighted")
    }
    
    if (!is.null(sparsity)) {
        sparsity <- check_double(sparsity)
        if (!is.null(max_docfreq) && !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document ",
                 "threshold, both should not be specified")
        if (verbose)
            catm("Note: converting sparsity into min_docfreq = 1 -", sparsity, ".\n")
        min_docfreq <- 1.0 - sparsity
        docfreq_type <- "prop"
    }
    
    f <- trim_features(colSums(x), docfreq(x), ndoc(x),
                       min_termfreq, max_termfreq, termfreq_type, 
                       min_docfreq, max_docfreq, docfreq_type)
    
    if (verbose)
        before <- stats_dfm(x)
    x <- dfm_select(x, f, valuetype = "fixed", case_insensitive = FALSE, 
                    verbose = FALSE)
    if (verbose)
        message_dfm("dfm_trim()", before, stats_dfm(x))    
    return(x)
}

trim_features <- function(termfreq, docfreq, n,
                          min_termfreq, max_termfreq, termfreq_type, 
                          min_docfreq, max_docfreq, docfreq_type) {
    
    s <- sum(termfreq)
    if (termfreq_type == "count") {
        if (!is.null(min_termfreq)) {
            min_termfreq <- check_integer(min_termfreq, min = 0)
        } else {
            min_termfreq <- 1
        }
        if (!is.null(max_termfreq)) {
            max_termfreq <- check_integer(max_termfreq, min = 0)
        } else {
            max_termfreq <- max(termfreq)
        }
    } else if (termfreq_type == "prop") {
        if (!is.null(min_termfreq)) {
            min_termfreq <- check_double(min_termfreq, min = 0, max = 1)
        } else {
            min_termfreq <- 0
        }
        if (!is.null(max_termfreq)) {
            max_termfreq <- check_double(max_termfreq, min = 0, max = 1)
        } else {
            max_termfreq <- 1
        }
        min_termfreq <- min_termfreq * s
        max_termfreq <- max_termfreq * s
    } else if (termfreq_type ==  "quantile")  {
        if (!is.null(min_termfreq)) {
            min_termfreq <- check_double(min_termfreq, min = 0, max = 1)
        } else {
            min_termfreq <- 0
        }
        if (!is.null(max_termfreq)) {
            max_termfreq <- check_double(max_termfreq, min = 0, max = 1)
        } else {
            max_termfreq <- 1
        }
        min_termfreq <- quantile(termfreq, min_termfreq, names = FALSE, type = 1)
        max_termfreq <- quantile(termfreq, max_termfreq, names = FALSE, type = 1)
    } else if (termfreq_type == "rank") {
        if (!is.null(min_termfreq)) {
            min_termfreq <- check_integer(min_termfreq, min = 1)
        } else {
            min_termfreq <- length(termfreq)
        }
        if (!is.null(max_termfreq)) {
            max_termfreq <- check_integer(max_termfreq, min = 1)
        } else {
            max_termfreq <- 1
        }
        r <- rank(termfreq * -1, ties.method = "min")
        min_termfreq <- min(termfreq[r <= min_termfreq])
        max_termfreq <- max(termfreq[r >= max_termfreq])
    }
    
    if (docfreq_type == "count") {
        if (!is.null(min_docfreq)) {
            min_docfreq <- check_integer(min_docfreq, min = 0)
        } else {
            min_docfreq <- 1
        }
        if (!is.null(max_docfreq)) {
            max_docfreq <- check_integer(max_docfreq, min = 0)
        } else {
            max_docfreq <- max(docfreq)
        }
    } else if (docfreq_type == "prop") {
        if (!is.null(min_docfreq)) {
            min_docfreq <- check_double(min_docfreq, min = 0, max = 1)
        } else {
            min_docfreq <- 0
        }
        if (!is.null(max_docfreq)) {
            max_docfreq <- check_double(max_docfreq, min = 0, max = 1)
        } else {
            max_docfreq <- 1
        }
        min_docfreq <- min_docfreq * n
        max_docfreq <- max_docfreq * n
    } else if (docfreq_type ==  "quantile")  {
        if (!is.null(min_docfreq)) {
            min_docfreq <- check_double(min_docfreq, min = 0, max = 1)
        } else {
            min_docfreq <- 0
        }
        if (!is.null(max_docfreq)) {
            max_docfreq <- check_double(max_docfreq, min = 0, max = 1)
        } else {
            max_docfreq <- 1
        }
        min_docfreq <- quantile(docfreq, min_docfreq, names = FALSE, type = 1)
        max_docfreq <- quantile(docfreq, max_docfreq, names = FALSE, type = 1)
    } else if (docfreq_type == "rank") {
        if (!is.null(min_docfreq)) {
            min_docfreq <- check_integer(min_docfreq, min = 1)
        } else {
            min_docfreq <- length(docfreq)
        }
        if (!is.null(max_docfreq)) {
            max_docfreq <- check_integer(max_docfreq, min = 1)
        } else {
            max_docfreq <- 1
        }
        r <- rank(docfreq * -1, ties.method = "min")
        min_docfreq <- min(docfreq[r <= min_docfreq])
        max_docfreq <- max(docfreq[r >= max_docfreq])
    }
    
    b1 <- termfreq < min_termfreq
    b2 <- termfreq > max_termfreq
    b3 <- docfreq < min_docfreq
    b4 <- docfreq > max_docfreq
    b <- b1 | b2 | b3 | b4
    
    return(names(b[!b]))
}
