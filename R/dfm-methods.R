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
                  if (verbose) catm("Note: converting sparsity into minDoc = 1 -", sparsity, "=", minDoc, ".\n")
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
                  catm("Removing features occurring fewer than ", messageMinCount, minCount, " times: ", 
                      nfeature(x) - length(featIndexAboveMinCount), "\n", sep = "")
              
              featIndexAboveMinDoc <- which(docfreq(x) >= minDoc)
              if (verbose & minDoc != 1)
                  catm("Removing features occurring in fewer than ", messageMinDoc, minDoc, " documents: ", 
                      nfeature(x) - length(featIndexAboveMinDoc), "\n", sep = "")

              if (minCount == 1 & minDoc == 1) {
                  catm("No features removed.", appendLF = TRUE)
                  return(x)
              }

              featureKeepIndex <- intersect(featIndexAboveMinCount, featIndexAboveMinDoc)
              if (length(featureKeepIndex)==0)  stop("No features left after trimming.")
              
              x <- x[, featureKeepIndex]
              
              if (!is.null(nsample)) {
                  if (nsample > nfeature(x))
                      catm("Note: retained features smaller in number than sample size so resetting nsample to nfeature.\n")
                  nsample <- min(nfeature(x), nsample)
                  # x <- x[, sample(1:nsample)]
                  x <- sample(x, size = nsample, what = "features")
                  if (verbose) catm("Retaining a random sample of", nsample, "words\n")
              }
              
              sort(x)
          })

#' @rdname trim
#' @param ... only included to allow legacy \code{trimdfm} to pass arguments to \code{trim}
#' @export
trimdfm <- function(x, ...) {
    catm("note: trimdfm deprecated: use trim instead.\n")
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
#' inaugDfm <- dfm(inaugTexts, verbose = FALSE)
#' 
#' # first 50 features (in original text order)
#' head(features(inaugDfm), 50)
#' 
#' # first 50 features alphabetically
#' head(sort(features(inaugDfm)), 50)
#' 
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(inaugDfm, 50))
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
topfeatures.dfm <- function(x, n = 10, decreasing = TRUE, ci = .95, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    if (n > nfeature(x)) n <- nfeature(x)
    if (is.resampled(x)) {
        subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
        subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
        return(data.frame(#features=colnames(subdfm),
            freq=colSums(subdfm[,,1]),
            cilo = apply(colSums(subdfm), 1, stats::quantile, (1-ci)/2),
            cihi = apply(colSums(subdfm), 1, stats::quantile, 1-(1-ci)/2)))
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


#' compress a dfm by combining similarly named dimensions
#' 
#' "Compresses" a dfm whose dimension names are the same, for either documents 
#' or features.  This may happen, for instance, if features are made equivalent 
#' through application of a thesaurus.  It may also occur after lower-casing or 
#' stemming the features of a dfm, but this should only be done in very rare 
#' cases (approaching never: it's better to do this \emph{before} constructing 
#' the dfm.)  It could also be needed , after a \code{\link{cbind.dfm}} or 
#' \code{\link{rbind.dfm}} operation.
#' 
#' @param x input object, a \link{dfm}
#' @param margin character indicating which margin to compress on, either
#'   \code{"documents"}, \code{"features"}, or \code{"both"} (default)
#' @param ... additional arguments passed from generic to specific methods
#' @export
compress <- function(x, ...)  UseMethod("compress")

#' @rdname compress
#' @examples 
#' mat <- rbind(dfm(c("b A A", "C C a b B"), toLower = FALSE, verbose = FALSE),
#'              dfm("A C C C C C", toLower = FALSE, verbose = FALSE))
#' colnames(mat) <- toLower(features(mat))
#' mat
#' compress(mat, margin = "documents")
#' compress(mat, margin = "features")
#' compress(mat)
#' 
#' # no effect if no compression needed
#' compress(dfm(inaugTexts, verbose = FALSE))
#' @export
compress.dfm <- function(x, margin = c("both", "documents", "features"), ...) {
    margin <- match.arg(margin)
    
    uniquednames <- unique(rownames(x))
    uniquefnames <- unique(colnames(x))
    if (length(uniquednames) == nrow(x) & length(uniquefnames) == ncol(x)) 
        return(x)

    # add 1 since stored from 0, but constructor requires indexing from 1
    new_i <- x@i + 1
    new_j <- as(x, "dgTMatrix")@j + 1

    allZeroFeatures <- match(names(which(colSums(x)==0)), uniquefnames)
        
    # combine documents
    if (margin %in% c("both", "documents") & length(uniquednames) < nrow(x))
        new_i <- match(rownames(x), uniquednames)[new_i]
    else
        uniquednames <- rownames(x)

    # combine features
    if (margin %in% c("both", "features") & length(uniquefnames) < ncol(x))
        new_j <- match(colnames(x), uniquefnames)[new_j]
    else
        uniquefnames <- colnames(x)

    if (nf <- length(allZeroFeatures)) {
        new_i <- c(new_i, rep(1, nf))
        new_j <- c(new_j, allZeroFeatures)
    }
        
    new("dfmSparse", sparseMatrix(i = new_i, j = new_j, 
                                  x = c(x@x, rep(0, length(allZeroFeatures))),
                                  dimnames = list(docs = uniquednames, features = uniquefnames)),
        settings = x@settings,
        weightTf = x@weightTf,
        weightDf = x@weightDf,
        smooth = x@smooth,
        ngrams = x@ngrams,
        concatenator = x@concatenator)
} 

# rnames <- colnames(x)
# dnames <- rownames(x)
# microbenchmark::microbenchmark(m1 = t(crossprod(x, Matrix(sapply(unique(dnames),"==", dnames)))),
#                                m2 = t(t(x) %*% Matrix(sapply(unique(dnames),"==", dnames))), 
#                                times = 100)
 
