# dfm_weight -------------

#' Weight the feature frequencies in a dfm

#' @param x document-feature matrix created by \link{dfm}
#' @param scheme a label of the weight type:
#' \describe{
#'   \item{\code{count}}{\eqn{tf_{ij}}, an integer feature count (default when a dfm is created)}
#'   \item{\code{prop}}{the proportion of the feature counts of total feature
#'   counts (aka relative frequency), calculated as \eqn{tf_{ij} / \sum_j tf_{ij}}}
#'   \item{\code{propmax}}{the proportion of the feature counts of the highest
#'   feature count in a document, \eqn{tf_{ij} / \textrm{max}_j tf_{ij}}}
#'   \item{\code{logcount}}{take the logarithm of 1 + each count, for the given
#'   base: \eqn{\textrm{log}_{base}(1 + tf_{ij})}}
#'   \item{\code{boolean}}{recode all non-zero counts as 1}
#'   \item{\code{augmented}}{equivalent to \eqn{K + (1 - K) *} \code{dfm_weight(x,
#'   "propmax")}}
#'   \item{\code{logave}}{1 + the log of the counts) / (1 + log of the counts / the average count within document), or
#'   \deqn{\frac{1 + \textrm{log}_{base} tf_{ij}}{1 + \textrm{log}_{base}(\sum_j tf_{ij} / N_i)}}}
#' }
#' @param weights if \code{scheme} is unused, then \code{weights} can be a named
#'   numeric vector of weights to be applied to the dfm, where the names of the
#'   vector correspond to feature labels of the dfm, and the weights will be
#'   applied as multipliers to the existing feature counts for the corresponding
#'   named features.  Any features not named will be assigned a weight of 1.0
#'   (meaning they will be unchanged).
#' @param base base for the logarithm when \code{scheme} is \code{"logcount"} or 
#'   \code{logave}
#' @param K the K for the augmentation when \code{scheme = "augmented"}
#' @return \code{dfm_weight} returns the dfm with weighted values.  Note the
#'   because the default weighting scheme is \code{"count"}, simply calling this
#'   function on an unweighted dfm will return the same object.  Many users will
#'   want the normalized dfm consisting of the proportions of the feature counts
#'   within each document, which requires setting \code{scheme = "prop"}.
#' @export
#' @seealso \code{\link{dfm_tfidf}}, \code{\link{docfreq}}
#' @keywords dfm
#' @examples
#' dfmat1 <- dfm(data_corpus_inaugural)
#' 
#' x <- apply(dfmat1, 1, function(tf) tf/max(tf))
#' topfeatures(dfmat1)
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
#' \dontshow{
#' dfmat7 <- dfm(data_corpus_inaugural[1:5])
#' for (w in  c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave")) {
#'     dfmat8 <- dfm_weight(dfmat7, w)
#'     cat("\n\n=== weight() TEST for:", w, "; class:", class(dfmat8), "\n")
#'     head(dfmat8)
#' }}
#' @references  Manning, C.D., Raghavan, P., & Schütze, H. (2008).
#'   \emph{An Introduction to Information Retrieval}. Cambridge: Cambridge University Press. 
#'   \url{https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf}
dfm_weight <- function(
    x, 
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"),
    weights = NULL,
    base = 10,
    K = 0.5) {
    UseMethod("dfm_weight")
}

#' @export
dfm_weight.default <- function(
    x, 
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"),
    weights = NULL,
    base = 10,
    K = 0.5) {
    stop(friendly_class_undefined_message(class(x), "dfm_weight"))
}

#' @export
dfm_weight.dfm <- function(
    x, 
    scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"),
    weights = NULL,
    base = 10,
    K = 0.5) {

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
        }
    }
    
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    
    ### for numeric weights
    if (!is.null(weights)) {
        if (!missing(scheme)) 
            warning("scheme is ignored when numeric weights are supplied", 
                    call. = FALSE)
        
        ignore <- !names(weights) %in% featnames(x)
        if (any(ignore)) {
            warning("dfm_weight(): ignoring ", format(sum(ignore), big.mark=","), 
                    " unmatched weight feature", 
                    ifelse(sum(ignore) == 1, "", "s"), 
                    noBreaks. = TRUE, call. = FALSE)
        }
        
        weight <- rep(1, nfeat(x)) 
        names(weight) <- featnames(x)
        weights <- weights[!ignore]
        weight[match(names(weights), names(weight))] <- weights
        weight <- diag(weight)
        colnames(weight) <- colnames(x)
        return(as.dfm(x %*% weight))
        
    ### for scheme weights
    } else {      
        scheme <- match.arg(scheme)
        args <- as.list(match.call(expand.dots = FALSE))
        
        if ("K" %in% names(args) && scheme != "augmented")
            warning("K not used for this scheme")
        if (K < 0 | K > 1.0)
            stop("K must be in the [0, 1] interval")
        
        if (x@weightTf[["scheme"]] != "count")
            stop("this dfm has already been term weighted as: ", 
                 x@weightTf[["scheme"]])
        
        if (scheme == "count") {
            return(x)
            
        } else if (scheme == "prop") {
            div <- rowSums(x)
            x@x <- x@x / div[x@i+1]
            
        } else if (scheme == "propmax") {
            div <- maxtf(x)
            x@x <- x@x / div[x@i+1]
            
        } else if (scheme == "boolean") {
            x@x <- as.numeric(x@x > 0)
            
        } else if (scheme == "logcount") {
            x@x <- 1 + log(x@x, base)
            #x@x <- log(x@x + 1, base)
            x@x[is.infinite(x@x)] <- 0
            x@weightTf[["base"]] <- base
            
        } else if (scheme == "augmented") {
            maxtf <- maxtf(x)
            x@x <- K + (1 - K) * x@x / maxtf[x@i+1]
            x@weightTf[["K"]] <- K
            
        } else if (scheme == "logave") {
            meantf <- Matrix::rowSums(x) / Matrix::rowSums(dfm_weight(x, "boolean"))
            x@x <- (1 + log(x@x, base)) / (1 + log(meantf[x@i+1], base))
            x@weightTf[["base"]] <- base
            
        } else stop("invalid scheme")
        
        x@weightTf[["scheme"]] <- scheme
        return(x)
    }
}

#' deprecated name for dfm_weight
#' 
#' Deprecated function name for \code{\link{dfm_weight}}. 
#' @param ... arguments passed to \code{\link{dfm_weight}}
#' @export
#' @keywords internal deprecated
tf <- function(...) {
    .Deprecated("dfm_weight")
    UseMethod("dfm_weight")
}



# dfm_smooth --------------

#' @rdname dfm_weight
#' @param smoothing constant added to the dfm cells for smoothing, default is 1
#' @return \code{dfm_smooth} returns a dfm whose values have been smoothed by
#'   adding the \code{smoothing} amount. Note that this effectively converts a
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
    x@smooth <- x@smooth + smoothing
    x + smoothing
}

# docfreq -------------

#' Compute the (weighted) document frequency of a feature
#' 
#' For a \link{dfm} object, returns a (weighted) document frequency for each
#' term.  The default is a simple count of the number of documents in which a
#' feature occurs more than a given frequency threshold.  (The default threshold
#' is  zero, meaning that any feature occurring at least once in a document will
#' be counted.)
#' @param x a \link{dfm}
#' @param scheme type of document frequency weighting, computed as
#' follows, where \eqn{N} is defined as the number of documents in the dfm and
#' \eqn{s} is the smoothing constant:
#' \describe{
#' \item{\code{count}}{\eqn{df_j}, the number of documents for which \eqn{n_{ij} > threshold}}
#' \item{\code{inverse}}{\deqn{\textrm{log}_{base}\left(s + \frac{N}{k + df_j}\right)}}
#' \item{\code{inversemax}}{\deqn{\textrm{log}_{base}\left(s + \frac{\textrm{max}(df_j)}{k + df_j}\right)}}
#' \item{\code{inverseprob}}{\deqn{\textrm{log}_{base}\left(\frac{N - df_j}{k + df_j}\right)}}
#' \item{\code{unary}}{1 for each feature}
#' }
#' @param smoothing added to the quotient before taking the logarithm
#' @param k added to the denominator in the "inverse" weighting types, to 
#'   prevent a zero document count for a term
#' @param base the base with respect to which logarithms in the inverse document
#' frequency weightings are computed; default is 10 (see Manning, 
#'   Raghavan, and Schütze 2008, p123).
#' @param threshold numeric value of the threshold \emph{above which} a feature 
#'   will considered in the computation of document frequency.  The default is 
#'   0, meaning that a feature's document frequency will be the number of 
#'   documents in which it occurs greater than zero times.
#' @param use.names	logical; if \code{TRUE} attach feature labels as names of 
#'   the resulting numeric vector
#' @param ... not used
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
#'   \emph{Introduction to Information Retrieval}. Cambridge: Cambridge University Press.
#'   \url{https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf}
docfreq <- function(x, scheme = c("count", "inverse", "inversemax", 
                                  "inverseprob", "unary"),
                    smoothing = 0, k = 0, base = 10, threshold = 0, 
                    use.names = TRUE) {
    UseMethod("docfreq")
}

#' @export
docfreq.default <- function(x, scheme = c("count", "inverse", "inversemax", 
                                          "inverseprob", "unary"),
                        smoothing = 0, k = 0, base = 10, threshold = 0, 
                        use.names = TRUE) {
    stop(friendly_class_undefined_message(class(x), "docfreq"))
}

    
#' @export
docfreq.dfm <- function(x, scheme = c("count", "inverse", "inversemax", 
                                      "inverseprob", "unary"),
                        smoothing = 0, k = 0, base = 10, threshold = 0, 
                        use.names = TRUE) {
    
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
    # if (x@weightDf[["scheme"]] != "unary")
    #     stop("this dfm has already been term weighted as: ", x@weightDf[[1]])
    
    if (scheme == "unary") {
        result <- rep(1, nfeat(x))
    } else if (scheme == "count") {
        result <- colSums(x > threshold)
    } else if (scheme == "inverse") {
        result <- log(smoothing + (ndoc(x) / (k + docfreq(x, "count", 
                                                          use.names = FALSE))), 
                      base = base)
    } else if (scheme == "inversemax") {
        temp <- docfreq(x, "count", use.names = FALSE)
        result <- log(smoothing + (max(temp) / (k + temp)), base = base)
    } else if (scheme == "inverseprob") {
        temp <- docfreq(x, "count", use.names = FALSE)
        result <- pmax(0, log((ndoc(x) - temp) / (k + temp), base = base))
    }
    if (use.names) {
        names(result) <- featnames(x)
    } else {
        result <- unname(result)
    }
    result
}

# dfm_tfidf ---------------

#' Weight a dfm by \emph{tf-idf}
#' 
#' Weight a dfm by term frequency-inverse document frequency (\emph{tf-idf}), 
#' with full control over options.  Uses fully sparse methods for efficiency.
#' @param x object for which idf or tf-idf will be computed (a document-feature 
#'   matrix)
#' @param scheme_tf scheme for \code{\link{dfm_weight}}; defaults to \code{"count"}
#' @param scheme_df scheme for \code{\link{docfreq}}; defaults to
#'   \code{"inverse"}.  Other options to \code{\link{docfreq}} can be passed
#'   through the ellipsis (\code{...}).
#' @param base the base for the logarithms in the \code{\link{tf}} and
#'   \code{\link{docfreq}} calls; default is 10
#' @param ... additional arguments passed to \code{\link{docfreq}}.
#' @details \code{dfm_tfidf} computes term frequency-inverse document frequency
#'   weighting.  The default is to use counts instead of normalized term
#'   frequency (the relative term frequency within document), but this
#'   can be overridden using \code{scheme_tf = "prop"}.
#' @references Manning, C. D., Raghavan, P., & Schütze, H. (2008). 
#'   \emph{Introduction to Information Retrieval}. Cambridge: Cambridge University Press.
#'   \url{https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf}
#' @seealso \code{\link{dfm_weight}}, \code{\link{docfreq}}
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
dfm_tfidf <- function(x, scheme_tf = "count", scheme_df = "inverse", base = 10, ...) {
    UseMethod("dfm_tfidf")
}

#' @export
dfm_tfidf.default <- function(x, scheme_tf = "count", scheme_df = "inverse", base = 10, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_tfidf"))
}
    
#' @export
dfm_tfidf.dfm <- function(x, scheme_tf = "count", scheme_df = "inverse", base = 10, ...) {
    
    attrs <- attributes(x)
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    
    args <- list(...)
    check_dots(args, names(formals(docfreq)))
    
    dfreq <- docfreq(x, scheme = scheme_df, base = base, ...)
    tfreq <- dfm_weight(x, scheme = scheme_tf, base = base)
    
    if (nfeat(x) != length(dfreq)) 
        stop("missing some values in idf calculation")
    
    # get the document indexes
    j <- as(tfreq, "dgTMatrix")@j + 1
    
    # replace just the non-zero values by product with idf
    x@x <- tfreq@x * dfreq[j]
    
    # record attributes
    attributes(x, FALSE) <- attrs
    x@weightTf <- tfreq@weightTf
    x@weightDf <- c(list(scheme = scheme_df, base = base), args)
    return(x)
}

#' Deprecated form of \code{\link{dfm_tfidf}}
#' 
#' Deprecated function name for \emph{tf-idf} weighting of a
#' document-feature matrix.
#' @param ... arguments passed to \code{\link{dfm_tfidf}}
#' @keywords dfm internal deprecated
#' @export
tfidf <- function(...) {
    .Deprecated("dfm_tfidf")
    UseMethod("dfm_tfidf")
}

# internal --------------

## internal function to get maximum term frequency by document
## only applies to CsparseMatrix formats (dfm)
setGeneric("maxtf", function(x) standardGeneric("maxtf"))

setMethod("maxtf", signature(x = "dfm"), definition = function(x) {
    vapply(split(x@x, x@i), max, numeric(1))
})
