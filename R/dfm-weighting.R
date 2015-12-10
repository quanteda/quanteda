#' Weight the feature frequencies in a dfm by various methods
#' 
#' Returns a document by feature matrix with the feature frequencies weighted 
#' according to one of several common methods.
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param type The weighting function to aapply to the dfm. One of: \itemize{ 
#'   \item normTf - Length normalization: dividing the frequency of the feature 
#'   by the length of the document) \item logTf - The natural log of the term
#'   frequency \item tf-idf - Term-frequency * inverse document frequency. For a
#'   full explanation, see, for example, 
#'   \url{http://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html}.
#'    This implementation will not return negative values.  For finer-grained
#'   control, call \code{\link{tfidf}} directly. \item maxTf - The term
#'   frequency divided by the frequency of the most frequent term in the
#'   document \item ppmi -   Positive Pointwise Mutual Information }
#' @param smoothing amount to apply as additive smoothing to the
#'   document-feature matrix prior to weighting, default is 0.5, set to
#'   \code{smoothing=0} for no smoothing.
#' @param verbose if \code{TRUE} output status messages
#' @param ... not currently used
#' @return The dfm with weighted values
#' @export
#' @seealso \code{\link{tfidf}}
#' @author Paul Nulty and Kenneth Benoit
#' @examples
#' dtm <- dfm(inaugCorpus)
#' x <- apply(dtm, 1, function(tf) tf/max(tf))
#' topfeatures(dtm)
#' normDtm <- weight(dtm, "relFreq")
#' topfeatures(normDtm)
#' maxTfDtm <- weight(dtm, type="relMaxFreq")
#' topfeatures(maxTfDtm)
#' logTfDtm <- weight(dtm, type="logFreq")
#' topfeatures(logTfDtm)
#' tfidfDtm <- weight(dtm, type="tfidf")
#' topfeatures(tfidfDtm)
#' 
#' # combine these methods for more complex weightings, e.g. as in Section 6.4 of
#' # Introduction to Information Retrieval
#' head(logTfDtm <- weight(dtm, type="logFreq"))
#' head(tfidf(logTfDtm, normalize = FALSE))
#' 
#' @references Manning, Christopher D., Prabhakar Raghavan, and Hinrich Schutze.
#'   Introduction to information retrieval. Vol. 1. Cambridge: Cambridge 
#'   university press, 2008.
setGeneric("weight", function(x, ...) standardGeneric("weight"))

#' @rdname weight
#' @examples
#' \dontshow{
#' testdfm <- dfm(inaugTexts[1:5], verbose = FALSE)
#' for (w in c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf")) {
#'     testw <- weight(testdfm, w)
#'     cat("\n\n=== weight() TEST for:", w, "; class:", class(testw), "\n")
#'     head(testw)
#' }
#' }
setMethod("weight", signature = "dfm", 
          definition = function(x, type=c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf"), #, "ppmi"), 
                                smoothing = 0, verbose=TRUE, ...) {
              if (length(addedArgs <- list(...)))
                  warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
              type = match.arg(type)
              x <- x + smoothing
             if (x@weightTf[["scheme"]] != "count") {
                 cat("  No weighting applied: you should not weight an already weighted dfm.\n")
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
#               if (is(x, "dfm")) x@weighting <- type
              # x[is.infinite(x)] <- 0
          })


#' @rdname weight
#' @details \code{smoother(x, smoothing)} is a shortcut for \code{weight(x, "frequency", smoothing)}
#' @export
smoother <- function(x, smoothing) weight(x, "frequency", smoothing = smoothing)


#' @rdname weight
#' @export
setGeneric("weighting", function(object) standardGeneric("weighting"))

#' @rdname weight
#' @param object the dfm object for accessing the weighting setting
#' @details \code{weighting} queries (but cannot set) the weighting applied to the dfm.
#' @return \code{weighting} returns a character object describing the type of weighting applied to the dfm.
setMethod("weighting", signature(object="dfm"), function(object) {
    if (isS4(object)) 
        object@weighting
    else 
        attr(object, "weighting")
})


# have a separate method here to allow S3 dfm to still exist
# @rdname weight
#setMethod("weight", signature = "dfmDense", getMethod("weight", "dfmSparse"))



#' get the document frequency of a feature
#' 
#' For a \link{dfm-class} object, returns the number of documents in which a 
#' feature occurs more than a given frequency threshold.  The default is greater
#' than zero, meaning that a feature occurs at least once in a document.
#' @param x a \link{dfm-class} document-feature matrix
#' @param threshold numeric value of the threshold \emph{above which} a feature 
#'   will considered in the computation of document frequency.  The default is 
#'   0, meaning that a feature's document frequency will be the number of 
#'   documents in which it occurs greater than zero times.
#' @param USE.NAMES	logical; if \code{TRUE} attach feature labels as names of 
#'   the resulting numeric vector
#' @return a numeric vector of document frequencies for each feature
#' @export
#' @examples 
#' mydfm <- dfm(inaugTexts[1:2], verbose = FALSE)
#' docfreq(mydfm[, 1:20])
setGeneric("docfreq", function(x, threshold = 0, USE.NAMES = TRUE) standardGeneric("docfreq"))

#' @rdname docfreq
setMethod("docfreq", signature(x = "dfmSparse"), 
          function(x, threshold = 0, USE.NAMES = TRUE) {
              tx <- t(x)
              featfactor <- factor(tx@i, 0:(nfeature(x)-1), labels = features(x))
              result <- as.integer(table(featfactor[tx@x > threshold]))
              if (USE.NAMES) names(result) <- features(x)
              result
          })

#' @rdname docfreq
setMethod("docfreq", signature(x = "dfmDense"), 
          function(x, threshold = 0, USE.NAMES = TRUE) {
              if (!any(x@x <= threshold)) 
                  result <- rep(ndoc(x), nfeature(x))
              else
                  result <- colSums(as.matrix(x) > threshold)
              
              if (!USE.NAMES) 
                  names(result) <- NULL
              else 
                  names(result) <- features(x)
              
              result
          })



#' compute tf-idf weights from a dfm
#' 
#' Compute tf-idf, inverse document frequency, and relative term frequency on 
#' document-feature matrices.  See also \code{\link{weight}}.
#' @param x object for which idf or tf-idf will be computed (a document-feature 
#'   matrix)
#' @param normalize if \code{TRUE}, use relative term frequency
#' @param smoothing amount to apply as additive smoothing to the 
#'   document-feature matrix prior to weighting, default is \code{0} for no 
#'   smoothing.  Another sensible value would be 0.5.
#' @param ... additional arguments passed to \code{\link{idf}} when calling
#'   \code{tdidf}
#' @details \code{tfidf} computes term frequency-inverse document frequency 
#'   weighting.  The default is to normalize term frequency (by computing 
#'   relative term frequency within document) but this is not performed if 
#'   \code{normalize = FALSE}.
#' @references Manning, C. D., Raghavan, P., & Schutze, H. (2008). 
#'   \emph{Introduction to Information Retrieval}. Cambridge University Press.
#' @examples 
#' head(LBGexample[, 5:10])
#' head(tfidf(LBGexample)[, 5:10])
#' idf(LBGexample)[5:15]
#' head(tf(LBGexample)[, 5:10])
#' 
#' # replication of worked example from
#' # https://en.wikipedia.org/wiki/Tf–idf#Example_of_tf.E2.80.93idf
#' (wikiDfm <- new("dfmSparse", 
#'                 Matrix::Matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
#'                    byrow = TRUE, nrow = 2,  
#'                    dimnames = list(docs = c("document1", "document2"), 
#'                      features = c("this", "is", "a", "sample", "another",
#'                                   "example")), sparse = TRUE)))
#' idf(wikiDfm, k = 0)
#' tfidf(wikiDfm, normalize = FALSE, k = 0)
#' @export
tfidf <- function(x, ...) UseMethod("tfidf")


#' @rdname tfidf
#' @export
tfidf.dfm <- function(x, normalize = TRUE, smoothing = 0L, ...) {
    invdocfr <- idf(x, ...)
    if (smoothing > 0) x <- x + smoothing
    if (normalize) x <- tf(x, "prop")
    if (nfeature(x) != length(invdocfr)) 
        stop("missing some values in idf calculation")
    t(t(x) * invdocfr)
}


#' @rdname tfidf
#' @param k additional constant to add to the denominator in \emph{idf} 
#'   computation, default is \code{k = 1}
#' @param base the base with respect to which logarithms in \code{\link{idf}}
#'   and \code{\link{tf}}} computation are computed, default is 10 (see Manning,
#'   Raghavan, and Schutze 2008, p123).
#' @param USE.NAMES	logical; if \code{TRUE} attach feature labels as names of 
#'   the resulting numeric vector
#' @export
#' @details \code{idf} computes inverse document frequency with a constant 
#'   \code{k} added to the denominator of log document frequency.
#' @seealso \code{\link{docfreq}}, \code{\link{weight}}
idf <- function(x, k = 1, USE.NAMES = TRUE, base = 10) UseMethod("idf")

#' @rdname tfidf
#' @export
idf.dfm <- function(x, k = 1, USE.NAMES = TRUE, base = 10) {
    x <- log(ndoc(x), base = base) - log(docfreq(x, USE.NAMES = FALSE) + k, base = base)
    if (!USE.NAMES) names(x) <- NULL
    x
}

#' @rdname tfidf
#' @export
setGeneric("tf", 
           function(x, scheme = c("count", "prop", "propmax", "boolean", "log", "augmented", "logave"),
                    base = 10, K = 0.5)
               standardGeneric("tf"))

#' @rdname tfidf
#' @param scheme divisor for the normalization of feature frequencies by document.  Valid types include:
#' \describe{
#' \item{\code{unity}}{default, each feature count will remain as feature counts, 
#' equivalent to dividing by 1}
#' \item{\code{total}}{total number of features per document, so that the sum of the normalized feature
#' values is 1.0}
#' \item{\code{maxCount}}{maximum feature count per document}}
#' @details \code{tf} is a shortcut to compute relative term frequecies (identical to 
#' \code{\link{weight}(x, "relFreq")}).
#' @param base base for the logarithm when \code{scheme} is \code{"log"} or \code{logave}
#' @param K the K for the augmentation when \code{scheme = "augmented"}
#' @export
setMethod("tf", signature(x = "dfm"), definition = 
              function(x, scheme = c("count", "prop", "propmax", "boolean", "log", "augmented", "logave"),
                       base = 10, K = 0.5) {
                  
                  scheme <- match.arg(scheme)
                  args <- as.list(match.call(expand.dots=FALSE))
                  if ("base" %in% names(args) & !(scheme %in% c("log", "logave")))
                      warning("base not used for this scheme")
                  if ("K" %in% names(args) & scheme != "augmented")
                      warning("K not used for this scheme")
                  if (K < 0 | K > 1.0)
                      stop("K must be in the [0, 1] interval")
                  
                  if (x@weightTf[["scheme"]] != "count")
                      stop("this dfm has already been term weighted as:", x@tf)
                  
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
              })



## internal function to get maximum term frequency by document
## only applies to CsparseMatrix formats (dfmSparse)
setGeneric("maxtf", function(x) standardGeneric("maxtf"))

setMethod("maxtf", signature(x = "dfmSparse"), definition = function(x) {
    freq <- doc <- V1 <- NULL 
    dt <- data.table(doc = t(x)@i, freq = x@x)
    dt[, max(freq), by = doc][, V1]
    ## note: this is faster for small dfms:
    # sapply(split(x@x, x@i), max)
})

setMethod("maxtf", signature(x = "dfmDense"), definition = function(x) {
    apply(x, 1, max)
})




## BENCHMARKING

# microbenchmark(m1 <- apply(NSFdfm, 1, max), m2 <- NSFdfm[, max.col(t(NSFdfm))])

# require(data.table)
# f1 <- function(x) {
#     dt <- data.table(doc = t(x)@i, freq = x@x)
#     dt[, max(freq), by = doc][, V1]
# }
# 
# f2 <- function(x) {
#     sapply(split(x@x, x@i), max)
# }
# 
# myDfm <- dfm(inaugCorpus, verbose = FALSE)
# microbenchmark(f1(myDfm), f2(myDfm))
# microbenchmark(f1(NSFdfm), f2(NSFdfm), times = 5)

          