# dfm_weight -------------

#' Weight the feature frequencies in a dfm

#' @param x document-feature matrix created by [dfm]
#' @param scheme a label of the weight type:
#' \describe{
#'   \item{`count`}{\eqn{tf_{ij}}, an integer feature count (default when a dfm
#'   is created)}
#'   \item{`prop`}{the proportion of the feature counts of total feature counts
#'   (aka relative frequency), calculated as \eqn{tf_{ij} / \sum_j tf_{ij}}}
#'   \item{`propmax`}{the proportion of the feature counts of the highest
#'   feature count in a document, \eqn{tf_{ij} / \textrm{max}_j tf_{ij}}}
#'   \item{`logcount`}{take the 1 + the logarithm of each count, for the
#'   given base, or 0 if the count was zero: \eqn{1 +
#'   \textrm{log}_{base}(tf_{ij})} if \eqn{tf_{ij} > 0}, or 0 otherwise.}
#'   \item{`boolean`}{recode all non-zero counts as 1}
#'   \item{`augmented`}{equivalent to \eqn{k + (1 - k) *} `dfm_weight(x,
#'   "propmax")`}
#'   \item{`logave`}{(1 + the log of the counts) / (1 + log of the average count
#'   within document), or \deqn{\frac{1 + \textrm{log}_{base} tf_{ij}}{1 +
#'   \textrm{log}_{base}(\sum_j tf_{ij} / N_i)}}}
#'   \item{`logsmooth`}{log of the counts + `smooth`, or \eqn{tf_{ij} + s}}
#' }
#' @param weights if `scheme` is unused, then `weights` can be a named
#'   numeric vector of weights to be applied to the dfm, where the names of the
#'   vector correspond to feature labels of the dfm, and the weights will be
#'   applied as multipliers to the existing feature counts for the corresponding
#'   named features.  Any features not named will be assigned a weight of 1.0
#'   (meaning they will be unchanged).
#' @param base base for the logarithm when `scheme` is `"logcount"` or
#'   `logave`
#' @param k the k for the augmentation when `scheme = "augmented"`
#' @param force logical; if `TRUE`, apply weighting scheme even if the dfm
#'   has been weighted before.  This can result in invalid weights, such as as
#'   weighting by `"prop"` after applying `"logcount"`, or after
#'   having grouped a dfm using [dfm_group()].
#' @return `dfm_weight` returns the dfm with weighted values.  Note the
#'   because the default weighting scheme is `"count"`, simply calling this
#'   function on an unweighted dfm will return the same object.  Many users will
#'   want the normalized dfm consisting of the proportions of the feature counts
#'   within each document, which requires setting `scheme = "prop"`.
#' @export
#' @seealso [docfreq()]
#' @keywords dfm
#' @examples
#' dfmat1 <- dfm(data_corpus_inaugural)
#'
#' dfmat2 <- dfm_weight(dfmat1, scheme = "prop")
#' topfeatures(dfmat2)
#' dfmat3 <- dfm_weight(dfmat1)
#' topfeatures(dfmat3)
#' dfmat4 <- dfm_weight(dfmat1, scheme = "logcount")
#' topfeatures(dfmat4)
#' dfmat5 <- dfm_weight(dfmat1, scheme = "logave")
#' topfeatures(dfmat5)
#'
#' # combine these methods for more complex dfm_weightings, e.g. as in Section 6.4
#' # of Introduction to Information Retrieval
#' head(dfm_tfidf(dfmat1, scheme_tf = "logcount"))
#'
#' # apply numeric weights
#' str <- c("apple is better than banana", "banana banana apple much better")
#' (dfmat6 <- dfm(str, remove = stopwords("english")))
#' dfm_weight(dfmat6, weights = c(apple = 5, banana = 3, much = 0.5))
#'
#' @references  Manning, C.D., Raghavan, P., & Schütze, H. (2008).
#'   *An Introduction to Information Retrieval*. Cambridge: Cambridge University Press.
#'   <https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>
dfm_weight <- function(
    x,
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", 
               "logave", "logsmooth"),
    weights = NULL,
    base = 10,
    k = 0.5,
    smoothing = 0.5,
    force = FALSE) {
    UseMethod("dfm_weight")
}

#' @export
dfm_weight.default <- function(
    x,
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", 
               "logave", "logsmooth"),
    weights = NULL,
    base = 10,
    k = 0.5,
    smoothing = 0.5,
    force = FALSE) {
    stop(friendly_class_undefined_message(class(x), "dfm_weight"))
}

#' @export
dfm_weight.dfm <- function(
    x,
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"),
    weights = NULL,
    base = 10,
    k = 0.5,
    smoothing = 0.5,
    force = FALSE) {

    # traps for deprecated scheme values
    if (!missing(scheme)) {
        scheme <- tolower(scheme)
        if (scheme == "frequency") {
            .Deprecated(msg = 'scheme = "frequency" is deprecated; use dfm_weight(x, scheme = "count") instead')
            return(dfm_weight(x, scheme = "count", base = base, weights = weights))
        } else if (scheme == "relfreq") {
            .Deprecated(msg = 'scheme = "relfreq" is deprecated; use dfm_weight(x, scheme = "prop") instead')
            return(dfm_weight(x, scheme = "prop", base = base, weights = weights))
        } else if (scheme == "relmaxfreq") {
            .Deprecated(msg = 'scheme = "relmaxfreq" is deprecated; use dfm_weight(x, scheme = "propmax") instead')
            return(dfm_weight(x, scheme = "propmax", base = base, weights = weights))
        } else if (scheme == "logfreq") {
            .Deprecated(msg = 'scheme = "logfreq" is deprecated; use dfm_weight(x, scheme = "logcount") instead')
            return(dfm_weight(x, scheme = "logcount", base = base, weights = weights))
        } else if (scheme == "tfidf") {
            .Deprecated(msg = 'scheme = "tfidf" is deprecated; use dfm_tfidf(x) instead')
            return(dfm_tfidf(x, base = base))
        } else if (scheme == "logsmooth") {
            return(as.dfm(log(dfm_smooth(x, smoothing), base = base)))
        }
    }

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    attrs <- attributes(x)

    ### for numeric weights
    if (!is.null(weights)) {
        if (!missing(scheme))
            warning("scheme is ignored when numeric weights are supplied",
                    call. = FALSE)

        ignore <- !names(weights) %in% featnames(x)
        if (any(ignore)) {
            warning("dfm_weight(): ignoring ", format(sum(ignore), big.mark = ","),
                    " unmatched weight feature",
                    ifelse(sum(ignore) == 1, "", "s"),
                    noBreaks. = TRUE, call. = FALSE)
        }

        weight <- rep(1, nfeat(x))
        names(weight) <- featnames(x)
        weights <- weights[!ignore]
        weight[match(names(weights), names(weight))] <- weights

        weight <- Diagonal(x = weight)
        colnames(weight) <- colnames(x)
        return(as.dfm(x %*% weight))

    } else {
        ### for scheme weights
        scheme <- match.arg(scheme)
        args <- as.list(match.call(expand.dots = FALSE))

        if ("k" %in% names(args) && scheme != "augmented")
            warning("k not used for this scheme")
        if (k < 0 || k > 1.0)
            stop("k must be in the [0, 1] interval")

        if (!force) {
            if (field_object(attrs, "weight_tf")$scheme != "count" ||
                field_object(attrs, "weight_df")$scheme != "unary") {
                stop("will not weight a dfm already term-weighted as '",
                     field_object(attrs, "weight_tf")$scheme, "'; use force = TRUE to override",
                     call. = FALSE)
            }
        }

        if (scheme == "count") {
            return(x)

        } else if (scheme == "prop") {
            div <- rowSums(x)
            x@x <- x@x / div[x@i + 1]

        } else if (scheme == "propmax") {
            div <- maxtf(x)
            x@x <- x@x / div[x@i + 1]

        } else if (scheme == "boolean") {
            x@x <- as.numeric(x@x > 0)

        } else if (scheme == "logcount") {
            x@x <- 1 + log(x@x, base)
            x@x[is.infinite(x@x)] <- 0
            field_object(attrs, "weight_tf")$base <- base

        } else if (scheme == "augmented") {
            maxtf <- maxtf(x)
            x@x <- k + (1 - k) * x@x / maxtf[x@i + 1]
            field_object(attrs, "weight_tf")$k <- k

        } else if (scheme == "logave") {
            meantf <- Matrix::rowSums(x) / Matrix::rowSums(dfm_weight(x, "boolean"))
            x@x <- (1 + log(x@x, base)) / (1 + log(meantf[x@i + 1], base))
            field_object(attrs, "weight_tf")$base <- base
        }
        field_object(attrs, "weight_tf")$scheme <- scheme
        rebuild_dfm(x, attrs)
    }
}


# dfm_smooth --------------

#' @rdname dfm_weight
#' @param smoothing constant added to the dfm cells for smoothing, default is 1 
#'   for `dfm_smooth()` and 0.5 for `dfm_weight()`
#' @return `dfm_smooth` returns a dfm whose values have been smoothed by
#'   adding the `smoothing` amount. Note that this effectively converts a
#'   matrix from sparse to dense format, so may exceed memory requirements
#'   depending on the size of your input matrix.
#' @export
#' @examples
#' # smooth the dfm
#' dfmat <- dfm(data_corpus_inaugural)
#' dfm_smooth(dfmat, 0.5)
dfm_smooth <- function(x, smoothing = 1) {
    UseMethod("dfm_smooth")
}

#' @export
dfm_smooth.default <- function(x, smoothing = 1) {
    stop(friendly_class_undefined_message(class(x), "dfm_smooth"))
}

#' @export
dfm_smooth.dfm <- function(x, smoothing = 1) {
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    attrs <- attributes(x)
    field_object(attrs, "smooth") <- field_object(attrs, "smooth") + smoothing
    x <- x + smoothing
    rebuild_dfm(x, attrs)
}

# docfreq -------------

#' Compute the (weighted) document frequency of a feature
#'
#' For a [dfm] object, returns a (weighted) document frequency for each
#' term.  The default is a simple count of the number of documents in which a
#' feature occurs more than a given frequency threshold.  (The default threshold
#' is  zero, meaning that any feature occurring at least once in a document will
#' be counted.)
#' @param x a [dfm]
#' @param scheme type of document frequency weighting, computed as
#' follows, where \eqn{N} is defined as the number of documents in the dfm and
#' \eqn{s} is the smoothing constant:
#' \describe{
#' \item{`count`}{\eqn{df_j}, the number of documents for which \eqn{n_{ij} > threshold}}
#' \item{`inverse`}{\deqn{\textrm{log}_{base}\left(s + \frac{N}{k + df_j}\right)}}
#' \item{`inversemax`}{\deqn{\textrm{log}_{base}\left(s + \frac{\textrm{max}(df_j)}{k + df_j}\right)}}
#' \item{`inverseprob`}{\deqn{\textrm{log}_{base}\left(\frac{N - df_j}{k + df_j}\right)}}
#' \item{`unary`}{1 for each feature}
#' }
#' @param base the base with respect to which logarithms in the inverse document
#'   frequency weightings are computed; default is 10 (see Manning, Raghavan,
#'   and Schütze 2008, p123).
#' @param smoothing added to the quotient before taking the logarithm
#' @param k added to the denominator in the "inverse" weighting types, to 
#'   prevent a zero document count for a term
#' @param threshold numeric value of the threshold *above which* a feature
#'   will considered in the computation of document frequency.  The default is
#'   0, meaning that a feature's document frequency will be the number of
#'   documents in which it occurs greater than zero times.
#' @return a numeric vector of document frequencies for each feature
#' @keywords weighting dfm
#' @export
#' @examples
#' dfmat1 <- dfm(data_corpus_inaugural[1:2])
#' docfreq(dfmat1[, 1:20])
#'
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
#' dfmat2 <-
#'     matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'            byrow = TRUE, nrow = 2,
#'            dimnames = list(docs = c("document1", "document2"),
#'                            features = c("this", "is", "a", "sample",
#'                                         "another", "example"))) %>%
#'     as.dfm()
#' dfmat2
#' docfreq(dfmat2)
#' docfreq(dfmat2, scheme = "inverse")
#' docfreq(dfmat2, scheme = "inverse", k = 1, smoothing = 1)
#' docfreq(dfmat2, scheme = "unary")
#' docfreq(dfmat2, scheme = "inversemax")
#' docfreq(dfmat2, scheme = "inverseprob")
#' @references Manning, C. D., Raghavan, P., & Schütze, H. (2008).
#'   *Introduction to Information Retrieval*. Cambridge: Cambridge University Press.
#'   <https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>
docfreq <- function(x, scheme = c("count", "inverse", "inversemax",
                                  "inverseprob", "unary"),
                    base = 10, smoothing = 0, k = 0, threshold = 0) {
    UseMethod("docfreq")
}

#' @export
docfreq.default <- function(x, scheme = c("count", "inverse", "inversemax",
                                          "inverseprob", "unary"),
                    base = 10, smoothing = 0, k = 0, threshold = 0) {
    stop(friendly_class_undefined_message(class(x), "docfreq"))
}


#' @export
docfreq.dfm <- function(x, scheme = c("count", "inverse", "inversemax",
                                      "inverseprob", "unary"),
                    base = 10, smoothing = 0, k = 0, threshold = 0) {

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(numeric())
    scheme <- match.arg(scheme)
    args <- as.list(match.call(expand.dots = FALSE))
    if ("base" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("base not used for this scheme")
    if ("k" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("k not used for this scheme")
    if ("smoothing" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("smoothing not used for this scheme")
    if (k < 0)
        stop("k must be >= 0")

    if (scheme == "unary") {
        result <- rep(1, nfeat(x))
    } else if (scheme == "count") {
        result <- colSums(x > threshold)
    } else if (scheme == "inverse") {
        result <- log(smoothing + (ndoc(x) / (k + docfreq(x, "count"))), base = base)
    } else if (scheme == "inversemax") {
        temp <- docfreq(x, "count")
        result <- log(smoothing + (max(temp) / (k + temp)), base = base)
    } else if (scheme == "inverseprob") {
        temp <- docfreq(x, "count")
        result <- pmax(0, log((ndoc(x) - temp) / (k + temp), base = base))
    }
    names(result) <- featnames(x)
    return(result)
}

# featfreq -------------

#' Compute the frequencies of features
#'
#' For a [dfm] object, returns a frequency for each feature, computed
#' across all documents in the dfm. This is equivalent to `colSums(x)`.
#' @param x a [dfm]
#' @return a (named) numeric vector of feature frequencies
#' @keywords weighting dfm
#' @seealso [dfm_tfidf()], [dfm_weight()]
#' @export
#' @examples
#' dfmat <- dfm(data_char_sampletext)
#' featfreq(dfmat)
featfreq <- function(x) {
    UseMethod("featfreq")
}

#' @export
featfreq.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "featfreq"))
}

#' @export
featfreq.dfm <- function(x) {
    colSums(x)
}

# dfm_tfidf ---------------

#' Weight a dfm by *tf-idf*
#'
#' Weight a dfm by term frequency-inverse document frequency (*tf-idf*),
#' with full control over options.  Uses fully sparse methods for efficiency.
#' @param x object for which idf or tf-idf will be computed (a document-feature
#'   matrix)
#' @param scheme_tf scheme for [dfm_weight()]; defaults to `"count"`
#' @param scheme_df scheme for [docfreq()]; defaults to
#'   `"inverse"`.
#' @param base the base for the logarithms in the [dfm_weight()] and
#'   [docfreq()] calls; default is 10
#' @inheritParams dfm_weight
#' @param ... additional arguments passed to \code{\link{docfreq}}.
#' @details `dfm_tfidf` computes term frequency-inverse document frequency
#'   weighting.  The default is to use counts instead of normalized term
#'   frequency (the relative term frequency within document), but this
#'   can be overridden using `scheme_tf = "prop"`.
#' @references Manning, C. D., Raghavan, P., & Schütze, H. (2008).
#'   *Introduction to Information Retrieval*. Cambridge: Cambridge University Press.
#'   <https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>
#' @keywords dfm weighting
#' @examples
#' dfmat1 <- as.dfm(data_dfm_lbgexample)
#' head(dfmat1[, 5:10])
#' head(dfm_tfidf(dfmat1)[, 5:10])
#' docfreq(dfmat1)[5:15]
#' head(dfm_weight(dfmat1)[, 5:10])
#'
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
#' dfmat2 <-
#'     matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'            byrow = TRUE, nrow = 2,
#'            dimnames = list(docs = c("document1", "document2"),
#'                            features = c("this", "is", "a", "sample",
#'                                         "another", "example"))) %>%
#'     as.dfm()
#' dfmat2
#' docfreq(dfmat2)
#' dfm_tfidf(dfmat2, scheme_tf = "prop") %>% round(digits = 2)
#'
#' \dontrun{
#' # comparison with tm
#' if (requireNamespace("tm")) {
#'     convert(dfmat2, to = "tm") %>% tm::weightTfIdf() %>% as.matrix()
#'     # same as:
#'     dfm_tfidf(dfmat2, base = 2, scheme_tf = "prop")
#' }
#' }
#' @keywords dfm weighting
#' @export
dfm_tfidf <- function(x, scheme_tf = "count", scheme_df = "inverse",
                      base = 10, force = FALSE, ...) {
    UseMethod("dfm_tfidf")
}

#' @export
dfm_tfidf.default <- function(x, scheme_tf = "count", scheme_df = "inverse",
                              base = 10, force = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_tfidf"))
}

#' @export
dfm_tfidf.dfm <- function(x, scheme_tf = "count", scheme_df = "inverse",
                          base = 10, force = FALSE, ...) {

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)

    x <- dfm_weight(x, scheme = scheme_tf, base = base, force = force)
    v <- docfreq(x, scheme = scheme_df, base = base, ...)
    j <- as(x, "dgTMatrix")@j + 1L
    x@x <- x@x * v[j]
    attrs <- attributes(x)
    field_object(attrs, "weight_df") <- c(
        list(scheme = scheme_df,
             base = base),
        list(...)
    )
    rebuild_dfm(x, attrs)
}

# internal --------------

## internal function to get maximum term frequency by document
## only applies to CsparseMatrix formats (dfm)
setGeneric("maxtf", function(x) standardGeneric("maxtf"))

setMethod("maxtf", signature(x = "dfm"), definition = function(x) {
    vapply(split(x@x, x@i), max, numeric(1))
})
