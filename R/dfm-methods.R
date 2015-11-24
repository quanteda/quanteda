####################################################################
## methods for dfm objects
##
## Ken Benoit
####################################################################


#' Trim a dfm using threshold-based or random feature selection
#' 
#' Returns a document by feature matrix reduced in size based on document and 
#' term frequency, and/or subsampling.
#' @param x document-feature matrix of \link{dfm-class}
#' @param minCount minimum count or fraction of features in across all documents 
#' @param minDoc minimum number or fraction of documents in which a feature appears
#' @param nsample how many features to retain (based on random selection)
#' @param sparsity equivalent to 1 - minDoc, included for comparison with tm
#' @param verbose print messages
#' @return A \link{dfm-class} object reduced in features (with the same number 
#'   of documents)
#' @name trim
#' @export
#' @note Trimming a \link{dfm-class} object is an operation based on the values 
#'   in the document-feature \emph{matrix}.  To select subsets of a dfm based on
#'   attributes of the features themselves -- such as selecting features 
#'   matching a regular expression, or removing features matching a stopword 
#'   list, use \link{selectFeatures}.
#' @author Paul Nulty and Ken Benoit, some inspiration from Will Lowe's (see \code{trim} from the 
#'   \code{austin} package)
#' @seealso \code{\link{selectFeatures}}
#' @examples
#' (myDfm <- dfm(inaugCorpus, verbose = FALSE))
#' # only words occuring >=10 times and in >=2 docs
#' trim(myDfm, minCount = 10, minDoc = 2) 
#' # only words occuring >=10 times and in at least 0.4 of the documents
#' trim(myDfm, minCount = 10, minDoc = 0.4)
#' # only words occuring at least 0.01 times and in >=2 documents
#' trim(myDfm, minCount = .01, minDoc = 2)
#' # only words occuring 5 times in 1000
#' trim(myDfm, minDoc = 0.2, minCount = 0.005)
#' # sample 50 words occurring at least 20 times each
#' (myDfmSampled <- trim(myDfm, minCount = 20, nsample = 50))  
#' topfeatures(myDfmSampled)
#' \dontrun{
#' if (require(tm)) {
#'     (tmdtm <- convert(myDfm, "tm"))
#'     removeSparseTerms(tmdtm, 0.7)
#'     trim(td, minDoc = 0.3)
#'     trim(td, sparsity = 0.7)
#' }
#' }
#' @export
setGeneric("trim", 
           signature = c("x", "minCount", "minDoc", "sparsity", "nsample", "verbose"),
           def = function(x, minCount=1, minDoc=1, sparsity=NULL, nsample=NULL, verbose=TRUE)
               standardGeneric("trim"))

#' @rdname trim
setMethod("trim", signature(x = "dfm"), 
          function(x, minCount=1, minDoc=1, sparsity=NULL, nsample=NULL, verbose=TRUE) {
              stopifnot(minCount > 0, minDoc > 0)
              messageSparsity <- messageMinCount <- messageMinDoc <- ""
              if (!is.null(sparsity)) {
                  if (minDoc != 1)
                      stop("minDoc and sparsity both refer to a document threshold, both should not be specified")
                  minDoc <- (1 - sparsity)
                  if (verbose) cat("Note: converting sparsity into minDoc = 1 -", sparsity, "=", minDoc, ".\n")
              }             
              
              if (minCount < 1) {
                  messageMinCount <- paste0(minCount, " * ", nfeature(x), " = ")
                  minCount <- (nfeature(x) * minCount)
              }
              if (minDoc < 1) {
                  messageMinDoc <- paste0(minDoc, " * ", ndoc(x), " = ")
                  minDoc <- (ndoc(x) * minDoc)
              }
              featIndexAboveMinCount <- which(colSums(x) >= minCount, useNames = FALSE)
              if (verbose & minCount != 1)
                  cat("Removing features occurring fewer than ", messageMinCount, minCount, " times: ", 
                      nfeature(x) - length(featIndexAboveMinCount), "\n", sep = "")
              
              featIndexAboveMinDoc <- which(docfreq(x) >= minDoc)
              if (verbose & minDoc != 1)
                  cat("Removing features occurring in fewer than ", messageMinDoc, minDoc, " documents: ", 
                      nfeature(x) - length(featIndexAboveMinDoc), "\n", sep = "")

              featureKeepIndex <- intersect(featIndexAboveMinCount, featIndexAboveMinDoc)
              if (length(featureKeepIndex)==0)  stop("No features left after trimming.")
              
              x <- x[, featureKeepIndex]
              
              if (!is.null(nsample)) {
                  if (nsample > nfeature(x))
                      cat("Note: retained features smaller in number than sample size so resetting nsample to nfeature.\n")
                  nsample <- min(nfeature(x), nsample)
                  # x <- x[, sample(1:nsample)]
                  x <- sample(x, size = nsample, what = "features")
                  if (verbose) cat("Retaining a random sample of", nsample, "words\n")
              }
              
              sort(x)
          })

#' @rdname trim
#' @param ... only included to allow legacy \code{trimdfm} to pass arguments to \code{trim}
#' @export
trimdfm <- function(x, ...) {
    cat("note: trimdfm deprecated: use trim instead.\n")
    UseMethod("trim")
}



#' @export
#' @rdname ndoc
ndoc.dfm <- function(x) {
    nrow(x)
}




#' extract the feature labels from a dfm
#'
#' Extract the features from a document-feature matrix, which are stored as the column names
#' of the \link{dfm} object.
#' @param x the object (dfm) whose features will be extracted
#' @return Character vector of the features
#' @examples
#' features(dfm(inaugTexts))[1:50]  # first 50 features (alphabetically sorted)
#' @export
features <- function(x) {
    UseMethod("features")
}

#' @export
#' @rdname features
features.dfm <- function(x) {
    colnames(x)
}

#' @rdname docnames
#' @examples
#' # query the document names of a dfm
#' docnames(dfm(inaugTexts[1:5]))
#' @export
docnames.dfm <- function(x) {
    rownames(x)
}

#' @details \code{is.dfm} returns \code{TRUE} if and only if its argument is a \link{dfm}.
#' @rdname dfm
#' @export
is.dfm <- function(x) {
    is(x, "dfm")
    # "dfm" %in% class(x)
}

#' @details \code{as.dfm} coerces a matrix or data.frame to a dfm
#' @rdname dfm
#' @export
as.dfm <- function(x) {
    if (!any((c("matrix", "data.frame") %in% class(x))))
        stop("as.dfm only applicable to matrix(-like) objects.")
    new("dfmSparse", Matrix(as.matrix(x), sparse=TRUE))
    #     
    #     m <- as.matrix(x)
    #     attr(m, "settings") <- attr(x, "settings")
    #     attr(m, "weighting") <- attr(x, "weighting")
    #     class(m) <- class(x)
    #     m
}


#' sort a dfm by one or more margins
#'
#' Sorts a \link{dfm} by frequency of total features, total features in
#' documents, or both
#'
#' @param x Document-feature matrix created by \code{\link{dfm}}
#' @param margin which margin to sort on \code{features} to sort by frequency of
#'   features, \code{docs} to sort by total feature counts in documents, and
#'   \code{both} to sort by both
#' @param decreasing TRUE (default) if sort will be in descending order,
#'   otherwise sort in increasing order
#' @param ... additional arguments passed to base method \code{sort.int}
#' @return A sorted \link{dfm} matrix object
#' @export
#' @author Ken Benoit
#' @examples
#' dtm <- dfm(inaugCorpus)
#' dtm[1:10, 1:5]
#' dtm <- sort(dtm)
#' sort(dtm)[1:10, 1:5]
#' sort(dtm, TRUE, "both")[1:10, 1:5]  # note that the decreasing=TRUE argument
#'                                     # must be second, because of the order of the
#'                                     # formals in the generic method of sort()
sort.dfm <- function(x, decreasing=TRUE, margin = c("features", "docs", "both"), ...) {
    margin <- match.arg(margin)
    class_xorig <- class(x)
    if (margin=="features") {
        x <- x[, order(colSums(x), decreasing=decreasing)]
    } else if (margin=="docs") {
        x <- x[order(rowSums(x), decreasing=decreasing), ]
    } else if (margin=="both") {
        x <- x[order(rowSums(x), decreasing=decreasing),
               order(colSums(x), decreasing=decreasing)]
    }
    class(x) <- class_xorig
    return(x)
}


#' @rdname ndoc
#' @export
nfeature <- function(x) {
    UseMethod("nfeature")
}

#' @rdname ndoc
#' @export
nfeature.corpus <- function(x) {
    stop("nfeature not yet implemented for corpus objects.")
}

#' @rdname ndoc
#' @description \code{nfeature} is an alias for \code{ntype} when applied to dfm
#'   objects.  For a corpus or set of texts, "features" are only defined through
#'   tokenization, so you need to use \code{\link{ntoken}} to count these.
#' @export
#' @examples
#' nfeature(dfm(inaugCorpus))
#' nfeature(trim(dfm(inaugCorpus), minDoc=5, minCount=10))
nfeature.dfm <- function(x) {
    ncol(x)
}



#' Weight the feature frequencies in a dfm by various methods
#' 
#' Returns a document by feature matrix with the feature frequencies weighted 
#' according to one of several common methods. 
#' 
#' @param x document-feature matrix created by \link{dfm}
#' @param type The weighting function to aapply to the dfm. One of: 
#' \itemize{ 
#'   \item normTf - Length normalization: dividing the frequency of the feature 
#'   by the length of the document) 
#'   \item logTf - The natural log of the term frequency 
#'   \item tf-idf - Term-frequency * inverse 
#'   document frequency. For a full explanation, see, for example, 
#'   \url{http://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html}.
#'    This implementation will not return negative values. 
#'   \item maxTf - The term frequency divided 
#'   by the frequency of the most frequent term in the document 
#'   \item ppmi -   Positive Pointwise Mutual Information }
#' @param smoothing amount to apply as additive smoothing to the document-feature matrix prior to
#'    weighting, default is 0.5, set to \code{smoothing=0} for no smoothing.
#' @param normalize if \code{TRUE} (default) then normalize the dfm by relative
#'   term frequency prior to computing tfidf
#' @param verbose if \code{TRUE} output status messages
#' @param ... not currently used
#' @return The dfm with weighted values
#' @export
#' @author Paul Nulty and Kenneth Benoit
#' @examples
#' dtm <- dfm(inaugCorpus)
#' x <- apply(dtm, 1, function(tf) tf/max(tf))
#' topfeatures(dtm)
#' normDtm <- weight(dtm)
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
#' logTfDtm <- weight(dtm, type="logFreq")
#' wfidfDtm <- weight(logTfDtm, type="tfidf", normalize=FALSE)
#' 
#' @references Manning, Christopher D., Prabhakar Raghavan, and Hinrich Schutze.
#'   Introduction to information retrieval. Vol. 1. Cambridge: Cambridge 
#'   university press, 2008.
setGeneric("weight", function(x, ...) standardGeneric("weight"))

#' @rdname weight
#' @examples
#' \dontshow{
#' testdfm <- dfm(inaugTexts[1:5])
#' head(testdfm)
#' for (w in c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf")) {
#'     testw <- weight(testdfm, w)
#'     cat("\n\n=== weight() TEST for:", w, "; class:", class(testw), "\n")
#'     head(testw)
#' }
#' }
setMethod("weight", signature = "dfm", 
          definition = function(x, type=c("frequency", "relFreq", "relMaxFreq", "logFreq", "tfidf"), #, "ppmi"), 
                                smoothing = 0, normalize = TRUE, verbose=TRUE, ...) {
              type = match.arg(type)
              x <- x + smoothing
              if (weighting(x) != "frequency") {
                  cat("  No weighting applied: you should not weight an already weighted dfm.\n")
              } else if (type=="relFreq") {
                  ## UGLY HACK
                  if (is(x, "dfmSparse")) {
                      #tmp <- x/rowSums(x)
                      #tmp[is.na(tmp)] <- 0
                      x <- new("dfmSparse", x/rowSums(x))
                  } else 
                      if (is(x, "dfmDense"))
                          x <- new("dfmDense", x/rowSums(x))
                      else 
                          x <- x/rowSums(x)
              } else if (type=="relMaxFreq") {
                  x <- x / apply(x, 1, max)
              } else if (type=="logFreq") {
                  x <- log(x + ifelse(smoothing==0, 1, smoothing))
              } else if (type=="tfidf") {
                  # complicated as, is is to control coercion to a class for which logical operator is
                  # properly defined as a method, currently not dfm and child classes
                  idf <- log(ndoc(x)) - log(docfreq(x, smoothing))
                  if (normalize) x <- weight(x, "relFreq")
                  if (nfeature(x) != length(idf)) 
                      stop("missing some values in idf calculation")
                  # currently this strips the dfm of its special class, but this is a problem in 
                  # the t() method for dfms, not an issue with this operation
                  x <- new("dfmSparse", Matrix::Matrix(t(t(x) * idf)))
                  # class(x) <- c("dfm", class(x))
              }
              #               } else if (type=="ppmi") {
              #                   pij <- x/rowSums(x)
              #                   pij[is.na(pij)] <- 0
              #                   pi <- colSums(x)
              #                   pj <- rowSums(x)
              #                   pj[is.na(pj)] <- 0
              #                   pmi <- (pij / t(outer(pi,pj)))
              #                   x <- abs(pmi)
              #              } else warning(sprintf("Type %s not implmented, no weighting performed.", type))
              
              if (is(x, "dfm")) x@weighting <- type
              # x[is.infinite(x)] <- 0
              return(x)
          })

#' @rdname weight
#' @details \code{tf} is a shortcut for \code{weight(x, "relFreq")}
#' @export
tf <- function(x) {
    if (isS4(x))
        weight(x, "relFreq")
    else 
        x / rowSums(x)
}

#' @rdname weight
#' @details \code{tfidf} is a shortcut for \code{weight(x, "tfidf")}
#' @export
tfidf <- function(x) weight(x, "tfidf")

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
#' feature occurs more than a given frequency threshold.  The default is 
#' greater than zero, meaning that a feature occurs at least once in a document.
#' @param object a \link{dfm-class} document-feature matrix
#' @param threshold numeric value of the threshold \emph{above which} a feature
#'   will considered in the computation of document frequency.  The default is
#'   0, meaning that a feature's document frequency will be the number of
#'   documents in which it occurs greater than zero times.
#' @return a numeric vector of document frequencies for each feature
#' @export
#' @examples 
#' mydfm <- dfm(inaugTexts[1:2], verbose = FALSE)
#' docfreq(mydfm[, 1:20])
setGeneric("docfreq", signature = c("object", "threshold"), 
           def=function(object, threshold=0) standardGeneric("docfreq"))

## Note: need the coercions to dg[C,e]Matrix because > Op not currently 
## working for the dfmSparse,Dense classes

#' @rdname docfreq
setMethod("docfreq", signature(object="dfmDense", threshold="numeric"), 
          function(object, threshold=0) {
              tmp <- colSums(as(object, "dgeMatrix") > threshold)
              names(tmp) <- features(object)
              tmp
          })
#' @rdname docfreq
setMethod("docfreq", signature(object="dfmDense", threshold="missing"), 
          function(object, threshold=0) {
              tmp <- colSums(as(object, "dgeMatrix") > threshold)
              names(tmp) <- features(object)
              tmp
          })
#' @rdname docfreq
setMethod("docfreq", signature(object="dfmSparse", threshold="numeric"), 
          function(object, threshold=0) {
              tmp <- colSums(as(object, "dgCMatrix") > threshold)
              names(tmp) <- features(object)
              tmp
          })
#' @rdname docfreq
setMethod("docfreq", signature(object="dfmSparse", threshold="missing"), 
          function(object, threshold=0) {
              tmp <- colSums(as(object, "dgCMatrix") > threshold)
              names(tmp) <- features(object)
              tmp
          })
#' @rdname docfreq
setMethod("docfreq", signature(object="dfm", threshold="numeric"), 
          function(object, threshold=0) {
              tmp <- colSums(object > threshold)
              names(tmp) <- features(object)
              tmp
          })
#' @rdname docfreq
setMethod("docfreq", signature(object="dfm", threshold="missing"), 
          function(object, threshold=0) {
              tmp <- colSums(object > threshold)
              names(tmp) <- features(object)
              tmp
          })


# Additive smoothing of feature frequencies in a dfm
# 
# Smooths the feature counts by adding a small value (default 0.5) to remove
# zero counts. Zero counts are problematic for probability-based models.
# 
# @param x document-feature matrix created by \link{dfm}
# @param alpha The value to add to all counts. Default is 0.5
# @return The original dfm, with values weighted according to type function.
# @export
# @author Paul Nulty
# @examples
# dtm <- dfm(inaugCorpus)
# dtm[1:5,1:10]
# smDtm <- smoothdfm(dtm)
# smDtm[1:5,1:10]
smoothdfm <- function(x, alpha=0.5) {
    attr_orig <- attributes(x)
    x <- x + alpha
    attributes(x) <- attr_orig
    x
}


#' list the most frequent features
#'
#' List the most frequently occuring features in a \link{dfm}
#' @name topfeatures
#' @aliases topFeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If TRUE, return the \code{n} most frequent features, if
#'   FALSE, return the \code{n} least frequent features
#' @param ci confidence interval from 0-1.0 for use if dfm is resampled
#' @param ... additional arguments passed to other methods
#' @export
topfeatures <- function(x, ...) {
    UseMethod("topfeatures")
}

#' @return A named numeric vector of feature counts, where the names are the feature labels.
#' @examples
#' topfeatures(dfm(subset(inaugCorpus, Year>1980), verbose=FALSE))
#' topfeatures(dfm(subset(inaugCorpus, Year>1980), ignoredFeatures=stopwords("english"),
#'             verbose=FALSE))
#' # least frequent features
#' topfeatures(dfm(subset(inaugCorpus, Year>1980), verbose=FALSE), decreasing=FALSE)
#' @export
#' @rdname topfeatures
#' @importFrom stats quantile
topfeatures.dfm <- function(x, n=10, decreasing=TRUE, ci=.95, ...) {
    if (is.null(n)) n <- ncol(x)
    if (is.resampled(x)) {
        subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
        subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
        return(data.frame(#features=colnames(subdfm),
            freq=colSums(subdfm[,,1]),
            cilo=apply(colSums(subdfm), 1, stats::quantile, (1-ci)/2),
            cihi=apply(colSums(subdfm), 1, stats::quantile, 1-(1-ci)/2)))
    } else {
        subdfm <- sort(colSums(x), decreasing)
        return(subdfm[1:n])
    }
}

#' @export
#' @rdname topfeatures
topfeatures.dgCMatrix <- function(x, n=10, decreasing=TRUE, ...) {
    if (is.null(n)) n <- ncol(x)
    #     if (is.resampled(x)) {
    #         subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
    #         subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
    #         return(data.frame(#features=colnames(subdfm),
    #             freq=colSums(subdfm[,,1]),
    #             cilo=apply(colSums(subdfm), 1, quantile, (1-ci)/2),
    #             cihi=apply(colSums(subdfm), 1, quantile, 1-(1-ci)/2)))
    #     } else {
    
    csums <- colSums(x)
    names(csums) <- x@Dimnames$features
    subdfm <- sort(csums, decreasing)
    return(subdfm[1:n])
    #    }
}

#' @export
#' @param what dimension (of a \link{dfm}) to sample: can be \code{documents} or
#'   \code{features}
#' @return A dfm object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @seealso \code{\link{sample}}
#' @rdname sample
#' @examples
#' # sampling from a dfm
#' myDfm <- dfm(inaugTexts[1:10], verbose = FALSE)
#' sample(myDfm)[, 1:10]
#' sample(myDfm, replace = TRUE)[, 1:10]
#' sample(myDfm, what = "features")[1:10, ]
sample.dfm <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, 
                       what = c("documents", "features"), ...) {
    what <- match.arg(what)
    if (what == "documents") {
        if (size > ndoc(x))
            stop("size cannot exceed the number of documents (", ndoc(x), ")")
        x <- x[sample(ndoc(x), size, replace, prob), ]
    } else if (what == "features") {
        if (size > nfeature(x))
            stop("size cannot exceed the number of features (", nfeature(x), ")")
        x <- x[, sample(nfeature(x), size, replace, prob)]
    } else {
        stop("only documents or features please")
    }
    x
}

#' combine dfm object by columns
#' 
#' Combine two or more dfm objects by columns.  An implementation of
#' \code{\link{cbind}} for \link{dfm} objects. Calls \code{\link{cbind2}}
#' defined for object classes in the \pkg{Matrix} package.
#' @param x a \link{dfm} object
#' @param y a second \link{dfm} object to be joined column-wise to the first
#' @param ... optional arguments for methods
#' @return A dfm object with combined features from input dfm objects.  The attributes of this
#' new dfm are not currently preserved.
#' @export
#' @examples 
#' dfm1 <- dfm("This is a sample text.", verbose = FALSE)
#' dfm2 <- dfm("one two three", verbose = FALSE)
#' cbind(dfm1, dfm2)
cbind.dfm <- function(x, y, ...) {
    result <- Matrix::cbind2(x, y, ...)
    new("dfmSparse", result)
}
