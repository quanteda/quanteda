#' weight the feature frequencies in a dfm
#' 
#' Returns a document by feature matrix with the feature frequencies weighted 
#' according to one of several common methods.  Some shortcuts that offer finer-grained control are:
#' \itemize{
#' \item{\code{\link{tf}}}{  compute term frequency weights}
#' \item{\code{\link{tfidf}}}{  compute term frequency-inverse document frequency weights}
#' \item{\code{\link{docfreq}}}{  compute document frequencies of features}
#' }
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param type a label of the weight type:
#' \describe{
#'  \item{\code{"frequency"}}{integer feature count (default when a dfm is created)}
#'  \item{\code{"relFreq"}}{the proportion of the feature counts of total feature counts (aka relative frequency)}
#'  \item{\code{"relMaxFreq"}}{the proportion of the feature counts of the highest feature count in a document}
#'  \item{\code{"logFreq"}}{natural logarithm of the feature count}
#'  \item{\code{"tfidf"}}{Term-frequency * inverse document frequency. For a
#'   full explanation, see, for example, 
#'   \url{http://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html}.
#'    This implementation will not return negative values.  For finer-grained
#'   control, call \code{\link{tfidf}} directly.}
#'   }
#' @param weights if \code{type} is unused, then \code{weights} can be 
#'   a named numeric vector of weights to be applied to the dfm, 
#'   where the names of the vector correspond to feature labels of the dfm, and 
#'   the weights will be applied as multipliers to the existing feature counts 
#'   for the corresponding named fatures.  Any features not named will be 
#'   assigned a weight of 1.0 (meaning they will be unchanged).
#' @note For finer grained control, consider calling \code{\link{tf}} or 
#'   \code{\link{tfidf}} directly.
#' @return The dfm with weighted values.
#' @export
#' @seealso \code{\link{tf}},  \code{\link{tfidf}}, \code{\link{docfreq}}
#' @keywords dfm
#' @author Paul Nulty and Kenneth Benoit
#' @examples
#' dtm <- dfm(data_corpus_inaugural)
#' 
#' x <- apply(dtm, 1, function(tf) tf/max(tf))
#' topfeatures(dtm)
#' normDtm <- dfm_weight(dtm, "relFreq")
#' topfeatures(normDtm)
#' maxTfDtm <- dfm_weight(dtm, type = "relMaxFreq")
#' topfeatures(maxTfDtm)
#' logTfDtm <- dfm_weight(dtm, type = "logFreq")
#' topfeatures(logTfDtm)
#' tfidfDtm <- dfm_weight(dtm, type = "tfidf")
#' topfeatures(tfidfDtm)
#' 
#' # combine these methods for more complex dfm_weightings, e.g. as in Section 6.4
#' # of Introduction to Information Retrieval
#' head(logTfDtm <- dfm_weight(dtm, type = "logFreq"))
#' head(tfidf(logTfDtm, normalize = FALSE))
#' 
#' #' # apply numeric weights
#' str <- c("apple is better than banana", "banana banana apple much better")
#' (mydfm <- dfm(str, remove = stopwords("english")))
#' dfm_weight(mydfm, weights = c(apple = 5, banana = 3, much = 0.5))
#' 
#' \dontshow{
#' testdfm <- dfm(data_char_inaugural[1:5], verbose = FALSE)
#' for (w in c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf")) {
#'     testw <- dfm_weight(testdfm, w)
#'     cat("\n\n=== weight() TEST for:", w, "; class:", class(testw), "\n")
#'     head(testw)
#' }}
#' @references Manning, Christopher D., Prabhakar Raghavan, and Hinrich Schutze.
#'   \emph{Introduction to Information Retrieval}. Vol. 1. Cambridge: Cambridge 
#'   University Press, 2008.
#' @examples
dfm_weight <- function(x, 
                       type = c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf"),
                       weights = NULL) {
    if (!is.dfm(x))
        stop("x must be a dfm object")
    
    # for numeric weights
    if (!is.null(weights)) {
        if (!missing(type))
            warning("type is ignored when numeric weights are supplied")
        if (any(!(matchedWeights <- names(weights) %in% features(x)))) {
            warning("ignoring", names(weights)[matchedWeights], ", not a feature match")
            weights <- weights[matchedWeights]
        }
        
        ## set weighting slot/attribute -- NEED TO ADD
        
        # use name matching for indexing, sorts too, returns NA where no match is found
        weights <- weights[features(x)]
        # reassign 1 to non-matched NAs
        weights[is.na(weights)] <- 1
        return(x * rep(weights, each = nrow(x)))
        
    } else {
        # named type weights
        type <- match.arg(type)
        
        if (x@weightTf[["scheme"]] != "count") {
            catm("  No weighting applied: you should not weight an already weighted dfm.\n")
        } else if (type=="relFreq") {
            return(tf(x, "prop"))
        } else if (type=="relMaxFreq") {
            return(tf(x, "propmax"))
        } else if (type=="logFreq") {
            return(tf(x, "log"))
        } else if (type=="tfidf") {
            return(tfidf(x))
        } else if (type == "frequency") {
            return(x)
        } else stop("unknown weighting type")

    }
}


#' @rdname dfm_weight
#' @param smoothing constant added to the dfm cells for smoothing, default is 1
#' @details This converts a matrix from sparse to dense format, so may exceed memory
#' requirements depending on the size of your input matrix.
#' @export
#' @examples 
#' # smooth the dfm
#' dfm_smooth(mydfm, 0.5)
dfm_smooth <- function(x, smoothing = 1) {
    if (!is.dfm(x))
        stop("x must be a dfm object")
    x + smoothing
}


#' compute the (weighted) document frequency of a feature
#' 
#' For a \link{dfm-class} object, returns a (weighted) document frequency for 
#' each term.  The default is a simple count of the number of documents in which
#' a feature occurs more than a given frequency threshold.  (The default 
#' threshold is  zero, meaning that any feature occuring at least once in a 
#' document will be counted.)
#' @param x a \link{dfm-class} document-feature matrix
#' @param scheme type of document frequency weighting
#' @param smoothing added to the quotient before taking the logarithm
#' @param k added to the denominator in the "inverse" weighting types, to 
#'   prevent a zero document count for a term
#' @param base the base with respect to which logarithms in the inverse document
#' frequency weightings are computed; default is 10 (see Manning, 
#'   Raghavan, and Schutze 2008, p123).
#' @param threshold numeric value of the threshold \emph{above which} a feature 
#'   will considered in the computation of document frequency.  The default is 
#'   0, meaning that a feature's document frequency will be the number of 
#'   documents in which it occurs greater than zero times.
#' @param USE.NAMES	logical; if \code{TRUE} attach feature labels as names of 
#'   the resulting numeric vector
#' @param ... not used
#' @return a numeric vector of document frequencies for each feature
#' @keywords internal weighting dfm
#' @export
#' @examples 
#' mydfm <- dfm(data_char_inaugural[1:2], verbose = FALSE)
#' docfreq(mydfm[, 1:20])
#' 
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
#' wikiDfm <- new("dfmSparse", 
#'                Matrix::Matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'                               byrow = TRUE, nrow = 2,  
#'                               dimnames = list(docs = c("document1", "document2"),
#'                                               features = c("this", "is", "a", "sample", 
#'                                                            "another", "example")), 
#'                               sparse = TRUE))
#' wikiDfm
#' docfreq(wikiDfm)
#' docfreq(wikiDfm, scheme = "inverse")
#' docfreq(wikiDfm, scheme = "inverse", k = 1, smoothing = 1)
#' docfreq(wikiDfm, scheme = "unary")
#' docfreq(wikiDfm, scheme = "inversemax")
#' docfreq(wikiDfm, scheme = "inverseprob")
#' @references Manning, C. D., Raghavan, P., & Schutze, H. (2008). 
#'   \emph{Introduction to Information Retrieval}. Cambridge University Press.
docfreq <- function(x, scheme = c("count", "inverse", "inversemax", "inverseprob", "unary"),
                    smoothing = 0, k = 0, base = 10, threshold = 0, USE.NAMES = TRUE) {
    
    if (!is.dfm(x))
        stop("x must be a dfm object")
    
    scheme <- match.arg(scheme)
    args <- as.list(match.call(expand.dots=FALSE))
    if ("base" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("base not used for this scheme")
    if ("k" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("k not used for this scheme")
    if ("smoothing" %in% names(args) & !(substring(scheme, 1, 7) == "inverse"))
        warning("smoothing not used for this scheme")
    
    if (k < 0)
        stop("k must be >= 0")
    
    if (x@weightDf[["scheme"]] != "unary")
        stop("this dfm has already been term weighted as:", x@weightDf)
    
    if (scheme == "unary") {
        result <- rep(1, nfeature(x))
        
    } else if (scheme == "count") {
        if (is(x, "dfmSparse")) {
            tx <- t(x)
            featfactor <- factor(tx@i, 0:(nfeature(x)-1), labels = features(x))
            result <- as.integer(table(featfactor[tx@x > threshold]))
        } else {
            if (!any(x@x <= threshold)) 
                result <- rep(ndoc(x), nfeature(x))
            else
                result <- colSums(as.matrix(x) > threshold)
        }
        
    } else if (scheme == "inverse") {
        result <- log(smoothing + (ndoc(x) / (k + docfreq(x, "count", USE.NAMES = FALSE))), base = base)
        
    } else if (scheme == "inversemax") {
        dftmp <- docfreq(x, "count", USE.NAMES = FALSE)
        result <- log(smoothing + (max(dftmp) / (k + dftmp)), base = base)
        
    } else if (scheme == "inverseprob") {
        dftmp <- docfreq(x, "count", USE.NAMES = FALSE)
        result <- log((ndoc(x) - dftmp) / (k + dftmp), base = base)
        result[is.infinite(result)] <- 0
    }
    
    if (USE.NAMES) names(result) <- features(x)
    result
}



#' compute tf-idf weights from a dfm
#' 
#' Compute tf-idf, inverse document frequency, and relative term frequency on 
#' document-feature matrices.  See also \code{\link{weight}}.
#' @param x object for which idf or tf-idf will be computed (a document-feature 
#'   matrix)
#' @param normalize if \code{TRUE}, use relative term frequency
#' @param scheme scheme for \code{\link{docfreq}}
#' @param ... additional arguments passed to \code{\link{docfreq}} when calling
#'   \code{tfidf}
#' @details \code{tfidf} computes term frequency-inverse document frequency 
#'   weighting.  The default is not to normalize term frequency (by computing 
#'   relative term frequency within document) but this will be performed if 
#'   \code{normalize = TRUE}.  
#' @references Manning, C. D., Raghavan, P., & Schutze, H. (2008). 
#'   \emph{Introduction to Information Retrieval}. Cambridge University Press.
#' @keywords internal weighting dfm
#' @examples 
#' head(data_dfm_LBGexample[, 5:10])
#' head(tfidf(data_dfm_LBGexample)[, 5:10])
#' docfreq(data_dfm_LBGexample)[5:15]
#' head(tf(data_dfm_LBGexample)[, 5:10])
#' 
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
#' (wikiDfm <- new("dfmSparse", 
#'                 Matrix::Matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'                    byrow = TRUE, nrow = 2,  
#'                    dimnames = list(docs = c("document1", "document2"), 
#'                      features = c("this", "is", "a", "sample", "another",
#'                                   "example")), sparse = TRUE)))
#' docfreq(wikiDfm)
#' tfidf(wikiDfm)
#' @keywords internal weighting
#' @export
tfidf <- function(x, normalize = FALSE, scheme = "inverse", ...) {
    if (!is.dfm(x))
        stop("x must be a dfm object")
    invdocfr <- docfreq(x, scheme = scheme, ...)
    if (normalize) x <- tf(x, "prop")
    if (nfeature(x) != length(invdocfr)) 
        stop("missing some values in idf calculation")
    t(t(x) * invdocfr)
}



#' compute (weighted) term frequency from a dfm
#' 
#' Apply varieties of term frequency weightings to a \link{dfm}.
#' @param x object for which idf or tf-idf will be computed (a document-feature 
#'   matrix)
#' @param scheme divisor for the normalization of feature frequencies by 
#'   document.  Valid types include: 
#'   \describe{ 
#'   \item{\code{count}}{default, 
#'   each feature count will remain as feature counts, equivalent to dividing by
#'   1} 
#'   \item{\code{prop}}{feature proportions within document, equivalent to 
#'   dividing each term by the total count of features in the document.} 
#'   \item{\code{propmax}}{feature proportions relative to the most frequent
#'   term of the document, equivalent to dividing term counts by the frequency
#'   of the most frequent term in the document.} 
#'   \item{\code{boolean}}{recode all non-zero counts as 1} 
#'   \item{\code{log}}{take the logarithm of 1 + each
#'   count, for base \code{base}}
#'   \item{\code{augmented}}{equivalent to K + (1 - K) * \code{tf(x, "propmax")}}
#'   \item{\code{logave}}{(1 + the log of the counts) / (1 + log of the counts / the average count within document)} 
#'   }
#' @details \code{tf(x, scheme = "prop")} is equivalent to \code{\link{weight}(x, "relFreq")}).
#' @param base base for the logarithm when \code{scheme} is \code{"log"} or 
#'   \code{logave}
#' @param K the K for the augmentation when \code{scheme = "augmented"}
#' @return A document feature matrix to which the weighting scheme has been 
#'   applied.
#' @author Kenneth Benoit and Paul Nulty
#' @references Manning, C. D., Raghavan, P., & Schutze, H. (2008). 
#'   \emph{Introduction to Information Retrieval}. Cambridge University Press.
#'   
#'   \url{https://en.wikipedia.org/wiki/Tf-idf#Term_frequency_2}
#' @export
#' @keywords internal weighting dfm
tf <- function(x, scheme = c("count", "prop", "propmax", "boolean", "log", "augmented", "logave"),
               base = 10, K = 0.5) {
    if (!is.dfm(x))
        stop("x must be a dfm object")
    
    scheme <- match.arg(scheme)
    args <- as.list(match.call(expand.dots=FALSE))
    if ("base" %in% names(args) & !(scheme %in% c("log", "logave")))
        warning("base not used for this scheme")
    if ("K" %in% names(args) & scheme != "augmented")
        warning("K not used for this scheme")
    if (K < 0 | K > 1.0)
        stop("K must be in the [0, 1] interval")
    
    if (x@weightTf[["scheme"]] != "count")
        stop("this dfm has already been term weighted as:", x@weightTf)
    
    if (scheme == "count") {
        return(x)
        
    } else if (scheme == "prop") {
        div <- rowSums(x)
        if (is(x, "dfmSparse"))
            x@x <- x@x / div[x@i+1]
        else
            x <- x / div
        
    } else if (scheme == "propmax") {
        div <- maxtf(x)
        if (is(x, "dfmSparse"))
            x@x <- x@x / div[x@i+1]
        else 
            x <- x / div
        
    } else if (scheme == "boolean") {
        x@x <- as.numeric(x@x > 0)
        
    } else if (scheme == "log") {
        x@x <- 1 + log(x@x, base)
        x@x[is.infinite(x@x)] <- 0
        x@weightTf[["base"]] <- base
        
    } else if (scheme == "augmented") {
        maxtf <- maxtf(x)
        if (is(x, "dfmSparse"))
            x@x <- K + (1 - K) * x@x / maxtf[x@i+1]
        else
            x <- K + (1 - K) * x / maxtf
        x@weightTf[["K"]] <- K
        
    } else if (scheme == "logave") {
        meantf <- Matrix::rowMeans(x)
        if (is(x, "dfmSparse"))
            x@x <- (1 + log(x@x, base)) / (1 + log(meantf[x@i+1], base))
        else
            x <- (1 + log(x, base)) / (1 + log(meantf, base))
        x@weightTf[["base"]] <- base
        
    } else {
        stop("shouldn't be here!")
    }
    
    x@weightTf[["scheme"]] <- scheme
    return(x)
}



## internal function to get maximum term frequency by document
## only applies to CsparseMatrix formats (dfmSparse)
setGeneric("maxtf", function(x) standardGeneric("maxtf"))

setMethod("maxtf", signature(x = "dfmSparse"), definition = function(x) {
    freq <- doc <- V1 <- NULL 
#    dt <- data.table(doc = t(x)@i, freq = x@x)
    dt <- data.table(doc = x@i, freq = x@x)
    dt[, max(freq), by = doc][, V1]
    ## note: this is faster for small dfms:
    # sapply(split(x@x, x@i), max)
})

setMethod("maxtf", signature(x = "dfmDense"), definition = function(x) {
    apply(x, 1, max)
})


