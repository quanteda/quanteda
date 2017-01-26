
#' trim a dfm using frequency threshold-based feature selection
#' 
#' Returns a document by feature matrix reduced in size based on document and 
#' term frequency, usually in terms of a minimum frequencies, but may also be in
#' terms of maximum frequencies.  Setting a combination of minimum and maximum 
#' frequencies will select features based on a range.
#' @param x a \link{dfm} object
#' @param min_count,max_count minimum/maximum count or fraction of features across all documents,
#'   below/above which features will be removed
#' @param min_docfreq,max_docfreq minimum/maximum number or fraction of documents in which a feature
#'   appears, below/above which features will be removed
#' @param sparsity equivalent to 1 - min_docfreq, included for comparison with
#'   \pkg{tm}
#' @param verbose print messages
#' @return A \link{dfm} reduced in features (with the same number 
#'   of documents)
#' @export
#' @note Trimming a \link{dfm} object is an operation based on the values 
#'   in the document-feature \emph{matrix}.  To select subsets of a dfm based on
#'   attributes of the features themselves -- such as selecting features 
#'   matching a regular expression, or removing features matching a stopword 
#'   list, use \code{\link{dfm_select}}.
#' @author Ken Benoit and Paul Nulty, with some inspiration from Will Lowe's
#'   (see \code{trim} from the \code{austin} package)
#' @seealso \code{\link{dfm_select}}, \code{\link{dfm_sample}}
#' @examples
#' (myDfm <- dfm(data_corpus_inaugural[1:5]))
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
#' # keep only words occuring <=10 times and in at most 3/4 of the documents
#' dfm_trim(myDfm, max_count = 10, max_docfreq = 0.75)
#'
#' # keep only words occuring at least 0.01 times and in >=2 documents
#' dfm_trim(myDfm, min_count = .01, min_docfreq = 2)
#' 
#' # keep only words occuring 5 times in 1000, and in 2 of 5 of documents
#' dfm_trim(myDfm, min_docfreq = 0.4, min_count = 0.005)
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
dfm_trim.dfm <- function(x, min_count = 1, min_docfreq = 1, max_count = NULL, max_docfreq = NULL, sparsity = NULL, verbose = FALSE) {

    # if (missing(min_count) & missing(min_docfreq) & missing(max_count) & missing(max_docfreq) & missing(sparsity)) {
    #     catm("No features removed.", appendLF = TRUE)
    #     return(x)
    # }
    
    # initialize additional messages as empty strings
    messageSparsity <- messageMinCount <- messageMinDoc <- messageMaxCount <- messageMaxDoc <- ""

    if (!is.null(sparsity)) {
        if ((!missing(min_docfreq) | !missing(max_docfreq)) & !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document threshold, both should not be specified")
        min_docfreq <- (1 - sparsity)
        if (verbose) catm("Note: converting sparsity into min_docfreq = 1 -", sparsity, "=", 
                          format(min_docfreq, big.mark=","), ".\n")
    }             
    
    # default for max_count is the frequency count of the most frequent feature
    max_count2 <- max_count
    if (missing(max_count))
        max_count2 <- max(colSums(x))
    # default for max_docfreq is the highest document frequency of any feature
    max_docfreq2 <- max_docfreq
    if (missing(max_docfreq))
        max_docfreq2 <- max(docfreq(x))

    # convert fractions into counts
    if (min_count < 1) {
        messageMinCount <- paste0(format(min_count, big.mark=","), " * ", format(nfeature(x), big.mark=","), " = ")
        min_count <- (nfeature(x) * min_count)
    }
    if (min_docfreq < 1) {
        messageMinDoc <- paste0(format(min_docfreq, big.mark=","), " * ", format(ndoc(x), big.mark=","), " = ")
        min_docfreq <- (ndoc(x) * min_docfreq)
    }
    if (!missing(max_count) & max_count2 < 1) {
        messageMaxCount <- paste0(format(max_count2, big.mark=","), " * ", format(nfeature(x), big.mark=","), " = ")
        max_count2 <- (nfeature(x) * max_count2)
    }
    if (!missing(max_docfreq) & max_docfreq2 < 1) {
        messageMaxDoc <- paste0(format(max_docfreq2, big.mark=","), " * ", format(ndoc(x), big.mark=","), " = ")
        max_docfreq2 <- (ndoc(x) * max_docfreq2)
    }

    # checks that min is less than max
    if (max_count2 < min_count)
        stop("max_count must be >= min_count")
    if (max_docfreq2 < min_docfreq)
        stop("max_docfreq must be >= min_docfreq")

    featIndexMinCount <- which(colSums(x) < min_count, useNames = FALSE) 
    featIndexMaxCount <- which(colSums(x) > max_count2, useNames = FALSE) 
    featIndexMinDoc <- which(docfreq(x) < min_docfreq)
    featIndexMaxDoc <- which(docfreq(x) > max_docfreq2)
    
    # in case no features were removed as a result of filtering conditions
    if (!length(c(featIndexMinCount, featIndexMaxCount, featIndexMinDoc, featIndexMaxDoc))) {
        catm("No features removed.", appendLF = TRUE)
        return(x)
    }
    
    if (verbose)
        catm("Removing features occurring: ", appendLF = TRUE)
    
    # print messages about frequency count removal
    if (verbose & length(c(featIndexMinCount, featIndexMaxCount))) {
        if (length(featIndexMinCount)) {
            catm("  - fewer than ", messageMinCount, min_count, " time",
                 ifelse(min_count != 1, "s", ""), ": ", 
                 format(length(featIndexMinCount), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (length(featIndexMaxCount)) {
            catm("  - more than ", messageMaxCount, max_count2, " time",
                 ifelse(max_count2 != 1, "s", ""), ": ", 
                 format(length(featIndexMaxCount), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
        
    # print messages about docfreq removal
    if (verbose & length(c(featIndexMinDoc, featIndexMaxDoc))) {
        if (length(featIndexMinDoc)) {
            catm("  - in fewer than ", messageMinDoc, min_docfreq, " document", 
                 ifelse(min_count != 1, "s", ""), ": ", 
                 format(length(featIndexMinDoc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (length(featIndexMaxDoc)) {
            catm("  - in more than ", messageMaxDoc, max_docfreq2, " document", 
                 ifelse(max_docfreq2 != 1, "s", ""), ": ", 
                 format(length(featIndexMaxDoc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
    
    featureRemoveIndex <- union(union(featIndexMinCount, featIndexMinDoc), 
                                union(featIndexMaxCount, featIndexMaxDoc))
    if (verbose) {
        catm("  Total features removed: ", format(length(featureRemoveIndex), big.mark=","), 
             " (", format(length(featureRemoveIndex) / nfeature(x) * 100, digits = 3, nsmall = 1), "%).", 
             sep = "", appendLF = TRUE)
    }
    if ((nfeature(x) - length(featureRemoveIndex)) == 0)  
        stop("No features left after trimming.")
    
    dfm_sort(x[, -featureRemoveIndex])
}

