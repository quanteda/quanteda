
#' trim a dfm using threshold-based or random feature selection
#' 
#' Returns a document by feature matrix reduced in size based on document and 
#' term frequency, and/or subsampling.
#' @param x document-feature matrix of \link{dfm-class}
#' @param min_count minimum count or fraction of features in across all documents 
#' @param min_docfreq minimum number or fraction of documents in which a feature appears
#' @param sparsity equivalent to 1 - min_docfreq, included for comparison with tm
#' @param verbose print messages
#' @return A \link{dfm-class} object reduced in features (with the same number 
#'   of documents)
#' @export
#' @note Trimming a \link{dfm-class} object is an operation based on the values 
#'   in the document-feature \emph{matrix}.  To select subsets of a dfm based on
#'   attributes of the features themselves -- such as selecting features 
#'   matching a regular expression, or removing features matching a stopword 
#'   list, use \link{dfm_select}.
#' @author Paul Nulty and Ken Benoit, with some inspiration from Will Lowe's (see \code{trim} from the 
#'   \code{austin} package)
#' @seealso \code{\link{dfm_select}}, \code{\link{dfm_sample}}
#' @examples
#' (myDfm <- dfm(data_corpus_inaugural, verbose = FALSE))
#' 
#' # only words occuring >=10 times and in >=2 docs
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 2) 
#' 
#' # only words occuring >=10 times and in at least 0.4 of the documents
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 0.4)
#' 
#' # only words occuring at least 0.01 times and in >=2 documents
#' dfm_trim(myDfm, min_count = .01, min_docfreq = 2)
#' 
#' # only words occuring 5 times in 1000
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
dfm_trim <- function(x, min_count = 1, min_docfreq = 1, sparsity = NULL, verbose = TRUE) {

    if (!is.dfm(x)) stop("x must be a dfm class object")
    stopifnot(min_count > 0, min_docfreq > 0)
    messageSparsity <- messageMinCount <- messageMinDoc <- ""
    
    if (!is.null(sparsity)) {
        if (min_docfreq != 1)
            stop("min_docfreq and sparsity both refer to a document threshold, both should not be specified")
        min_docfreq <- (1 - sparsity)
        if (verbose) catm("Note: converting sparsity into min_docfreq = 1 -", sparsity, "=", min_docfreq, ".\n")
    }             
    
    if (min_count < 1) {
        messageMinCount <- paste0(min_count, " * ", nfeature(x), " = ")
        min_count <- (nfeature(x) * min_count)
    }
    if (min_docfreq < 1) {
        messageMinDoc <- paste0(min_docfreq, " * ", ndoc(x), " = ")
        min_docfreq <- (ndoc(x) * min_docfreq)
    }
    featIndexAboveMinCount <- which(colSums(x) >= min_count, useNames = FALSE)
    if (verbose & min_count != 1)
        catm("Removing features occurring fewer than ", messageMinCount, min_count, " times: ", 
             nfeature(x) - length(featIndexAboveMinCount), "\n", sep = "")
    
    featIndexAboveMinDoc <- which(docfreq(x) >= min_docfreq)
    if (verbose & min_docfreq != 1)
        catm("Removing features occurring in fewer than ", messageMinDoc, min_docfreq, " documents: ", 
             nfeature(x) - length(featIndexAboveMinDoc), "\n", sep = "")
    
    if (min_count == 1 & min_docfreq == 1) {
        catm("No features removed.", appendLF = TRUE)
        return(x)
    }
    
    featureKeepIndex <- intersect(featIndexAboveMinCount, featIndexAboveMinDoc)
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





