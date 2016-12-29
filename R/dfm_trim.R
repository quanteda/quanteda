
#' trim a dfm using frequency threshold-based feature selection
#' 
#' Returns a document by feature matrix reduced in size based on document and 
#' term frequency, usually in terms of a minimum frequencies, but may also be in
#' terms of maximum frequencies.  Setting a combination of minimum and maximum 
#' frequencies will select features based on a range.
#' @param x document-feature matrix of \link{dfm-class}
#' @param min_count minimum count or fraction of features across all documents,
#'   below which features will be removed
#' @param min_docfreq minimum number or fraction of documents in which a feature
#'   appears, below which features will be removed
#' @param max_count maximum count or fraction of features across all documents,
#'   above which features will be removed
#' @param max_docfreq maximum number or fraction of documents in which a feature
#'   appears, above which features will be removed
#' @param sparsity equivalent to 1 - min_docfreq, included for comparison with
#'   tm
#' @param verbose print messages
#' @return A \link{dfm-class} object reduced in features (with the same number 
#'   of documents)
#' @export
#' @note Trimming a \link{dfm-class} object is an operation based on the values 
#'   in the document-feature \emph{matrix}.  To select subsets of a dfm based on
#'   attributes of the features themselves -- such as selecting features 
#'   matching a regular expression, or removing features matching a stopword 
#'   list, use \link{dfm_select}.
#' @author Ken Benoit and Paul Nulty, with some inspiration from Will Lowe's
#'   (see \code{trim} from the \code{austin} package)
#' @seealso \code{\link{dfm_select}}, \code{\link{dfm_sample}}
#' @examples
#' (myDfm <- dfm(data_corpus_inaugural))
#' 
#' # keep only words occuring >=10 times and in >=2 docs
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 2) 
#' 
#' # keep only words occuring >=10 times and in at least 0.4 of the documents
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 0.4)
#' 
#' # keep only words occuring <=10 times and in <=2 docs
#' dfm_trim(myDfm, max_count = 10, max_docfreq = 2) 
#' 
#' # keep only words occuring <=10 times and in at most 0.4 of the documents
#' dfm_trim(myDfm, max_count = 10, max_docfreq = 0.4)

#' # keep only words occuring at least 0.01 times and in >=2 documents
#' dfm_trim(myDfm, min_count = .01, min_docfreq = 2)
#' 
#' # keep only words occuring 5 times in 1000
#' dfm_trim(myDfm, min_docfreq = 0.2, min_count = 0.005)
#' 
#' \dontrun{
#' # compare to removeSpareTerms from the tm package 
#' if (require(tm)) {
#'     (tmdtm <- convert(myDfm, "tm"))
#'     removeSparseTerms(tmdtm, 0.7)
#'     dfm_trim(td, min_docfreq = 0.3)
#'     dfm_trim(td, sparsity = 0.7)
#' }
#' }
#' @export
dfm_trim <- function(x, min_count = 1, min_docfreq = 1, max_count = NULL, max_docfreq = NULL, sparsity = NULL, verbose = TRUE) {
    UseMethod("dfm_trim")
}
 
#' @export
#' @rdname dfm_trim
#' @noRd
dfm_trim.dfm <- function(x, min_count = 1, min_docfreq = 1, max_count = max(colSums(x)), max_docfreq = max(docfreq(x)), sparsity = NULL, verbose = TRUE) {

    if (min_count == 1 & min_docfreq == 1 & missing(max_count) & missing(max_docfreq)) {
        catm("No features removed.", appendLF = TRUE)
        return(x)
    }
    
    # if (!is.dfm(x)) stop("x must be a dfm class object")
    stopifnot(min_count > 0, min_docfreq > 0, max_docfreq > 0, max_count > 0)
    messageSparsity <- messageCount <- messageDoc <- ""

    if (!is.null(sparsity)) {
        if ((min_docfreq != 1 | !is.missing(max_docfreq)) & !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document threshold, both should not be specified")
        min_docfreq <- (1 - sparsity)
        if (verbose) catm("Note: converting sparsity into min_docfreq = 1 -", sparsity, "=", min_docfreq, ".\n")
    }             
    
    if (min_count < 1) {
        messageCount <- paste0("fewer than ", min_count, " * ", nfeature(x), " = ")
        min_count <- (nfeature(x) * min_count)
    }
    if (min_docfreq < 1) {
        messageDoc <- paste0("fewer than", min_docfreq, " * ", ndoc(x), " = ")
        min_docfreq <- (ndoc(x) * min_docfreq)
    }
    
    if (max_count > 1) {
        messageCount <- paste0("more than ", max_count, " * ", nfeature(x), " = ")
        max_count <- (nfeature(x) * max_count)
    }
    if (max_docfreq > 1) {
        messageDoc <- paste0("more than", max_docfreq, " * ", ndoc(x), " = ")
        max_docfreq <- (ndoc(x) * max_docfreq)
    }

    if (max_count < min_count)
        stop("max_count must be >= min_count")
    
    if (max_docfreq < min_docfreq)
        stop("max_docfreq must be >= min_docfreq")
    
    featIndexCount <- which(colSums(x) >= min_count & colSums(x) <= max_count, useNames = FALSE) 
    if (verbose & length(featIndexCount))
        catm("Removing features occurring ", messageCount, min_count, " times: ", 
             nfeature(x) - length(featIndexCount), "\n", sep = "")
    
    featIndexDoc <- which(docfreq(x) >= min_docfreq & docfreq(x) <= max_docfreq)
    if (verbose & length(featIndexCount))
        catm("Removing features occurring in ", messageDoc, min_docfreq, " documents: ", 
             nfeature(x) - length(featIndexDoc), "\n", sep = "")
    
    featureKeepIndex <- intersect(featIndexCount, featIndexDoc)
    if (length(featureKeepIndex)==0)  stop("No features left after trimming.")
    
    x <- x[, featureKeepIndex]
    
    # if (!is.null(nsample)) {
    #     if (nsample > nfeature(x))
    #         catm("Note: retained features smaller in number than sample size so resetting nsample to nfeature.\n")
    #     nsample <- min(nfeature(x), nsample)
    #     # x <- x[, sample(1:nsample)]
    #     x <- sample(x, size = nsample, what = "features")
    #     if (verbose) catm("Retaining a random sample of", nsample, "words\n")
    # }
    
    dfm_sort(x)
}





