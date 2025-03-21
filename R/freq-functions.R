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
#' dfmat1 <- dfm(tokens(data_corpus_inaugural))
#' docfreq(dfmat1[, 1:20])
#'
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
#' dfmat2 <-
#'     matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'            byrow = TRUE, nrow = 2,
#'            dimnames = list(docs = c("document1", "document2"),
#'                            features = c("this", "is", "a", "sample",
#'                                         "another", "example"))) |>
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
    check_class(class(x), "docfreq")
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

#' @export
docfreq.tokens <- function(x) {
    docfreq(as.tokens_xptr(x))
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
#' dfmat <- dfm(tokens(data_char_sampletext))
#' featfreq(dfmat)
featfreq <- function(x) {
    UseMethod("featfreq")
}

#' @export
featfreq.default <- function(x) {
    check_class(class(x), "featfreq")
}

#' @export
featfreq.dfm <- function(x) {
    colSums(x)
}

#' @export
featfreq.tokens <- function(x) {
    featfreq(as.tokens_xptr(x))
}
