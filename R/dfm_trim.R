
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
#' @note Trimming a \link{dfm} object is an operation based on the \emph{values}
#'   in the document-feature matrix.  To select subsets of a dfm based on
#'   the features themselves (meaning the feature labels from \code{\link{featnames}}) -- such as those   
#'   matching a regular expression, or removing features matching a stopword 
#'   list, use \code{\link{dfm_select}}.
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
#' # compare to removeSparseTerms from the tm package 
#' if (require(tm)) {
#'     (tmdtm <- convert(myDfm, "tm"))
#'     removeSparseTerms(tmdtm, 0.7)
#'     dfm_trim(td, min_docfreq = 0.3)
#'     dfm_trim(td, sparsity = 0.7)
#' }
#' }
#' @export
dfm_trim <- function(x, min_count = 1, min_docfreq = 1, max_count = NULL, max_docfreq = NULL, 
                     sparsity = NULL, verbose = quanteda_options("verbose")) {
    UseMethod("dfm_trim")
}
 
#' @export
#' @rdname dfm_trim
#' @noRd
dfm_trim.dfm <- function(x, min_count = 1, min_docfreq = 1, max_count = NULL, max_docfreq = NULL, 
                         sparsity = NULL, verbose = quanteda_options("verbose")) {

    x <- as.dfm(x)
    
    # initialize additional messages as empty strings
    msg_sparsity <- msg_min_count <- msg_min_doc <- msg_max_count <- msg_max_doc <- ""

    if (!is.null(sparsity)) {
        if ((min_docfreq > 1 || !is.null(max_docfreq)) && !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document threshold, both should not be specified")
        if (verbose) 
            catm("Note: converting sparsity into min_docfreq = 1 -", sparsity, "=", format(min_docfreq, big.mark=","), ".\n")
        min_docfreq <- 1.0 - sparsity
    }             
    
    if (is.null(max_count))
        max_count <- max(colSums(x))
    if (is.null(max_docfreq))
        max_docfreq <- max(docfreq(x))

    # convert fractions into counts
    if (min_count < 1.0) {
        msg_min_count <- paste0(format(min_count, big.mark=","), " * ", format(nfeature(x), big.mark=","), " = ")
        min_count <- nfeature(x) * min_count
    }
    if (min_docfreq < 1.0) {
        msg_min_doc <- paste0(format(min_docfreq, big.mark=","), " * ", format(ndoc(x), big.mark=","), " = ")
        min_docfreq <- ndoc(x) * min_docfreq
    }
 
    if (max_count < 1.0) {
        msg_max_count <- paste0(format(max_count, big.mark=","), " * ", format(nfeature(x), big.mark=","), " = ")
        max_count <- nfeature(x) * max_count
    }
    if (max_docfreq < 1.0) {
        msg_max_doc <- paste0(format(max_docfreq, big.mark=","), " * ", format(ndoc(x), big.mark=","), " = ")
        max_docfreq <- ndoc(x) * max_docfreq
    }

    # checks that min is less than max
    if (max_count < min_count)
        stop("max_count must be >= min_count")
    if (max_docfreq < min_docfreq)
        stop("max_docfreq must be >= min_docfreq")
    
    flag_min_count <- colSums(x) < min_count
    flag_max_count <- colSums(x) > max_count
    flag_min_doc <- docfreq(x) < min_docfreq
    flag_max_doc <- docfreq(x) > max_docfreq
    flag_all <- flag_min_count | flag_max_count | flag_min_doc | flag_max_doc
    
    # in case no features were removed as a result of filtering conditions
    if (!sum(flag_all)) {
        if (verbose) catm("No features removed.", appendLF = TRUE)
        return(x)
    }
    
    if (verbose) catm("Removing features occurring: ", appendLF = TRUE)
    
    # print messages about frequency count removal
    if (verbose && (sum(flag_min_count) || sum(flag_max_count))) {
        if (sum(flag_min_count)) {
            catm("  - fewer than ", msg_min_count, min_count, " time",
                 if (min_count != 1L) "s" else "", ": ", 
                 format(sum(flag_min_count), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_count)) {
            catm("  - more than ", msg_max_count, max_count, " time",
                 if (max_count != 1L) "s" else "", ": ", 
                 format(sum(flag_max_count), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
        
    # print messages about docfreq removal
    if (verbose && (sum(flag_min_doc) || sum(flag_max_doc))) {
        if (sum(flag_min_doc)) {
            catm("  - in fewer than ", msg_min_doc, min_docfreq, " document", 
                 ifelse(min_count != 1, "s", ""), ": ", 
                 format(sum(flag_min_doc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_doc)) {
            catm("  - in more than ", msg_max_doc, max_docfreq, " document", 
                 ifelse(max_docfreq != 1, "s", ""), ": ", 
                 format(sum(flag_max_doc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
    
    if (verbose) {
        catm("  Total features removed: ", format(sum(flag_all), big.mark=","), 
             " (", format(length(featureRemoveIndex) / nfeature(x) * 100, digits = 3, nsmall = 1), "%).", 
             sep = "", appendLF = TRUE)
    }
       
    x[, !flag_all]
}

