#' Trim a dfm using frequency threshold-based feature selection
#'
#' Returns a document by feature matrix reduced in size based on document and
#' term frequency, usually in terms of a minimum frequencies, but may also be in
#' terms of maximum frequencies.  Setting a combination of minimum and maximum
#' frequencies will select features based on a range.
#' @param x a \link{dfm} object
#' @param min_count,max_count minimum/maximum count or percentile frequency of
#'   features across all documents, below/above which features will be removed
#' @param min_docfreq,max_docfreq minimum/maximum number or fraction of
#'   documents in which a feature appears, below/above which features will be
#'   removed
#' @param sparsity equivalent to 1 - min_docfreq, included for comparison with
#'   \pkg{tm}
#' @param verbose print messages
#' @return A \link{dfm} reduced in features (with the same number of documents)
#' @export
#' @note Trimming a \link{dfm} object is an operation based on the \emph{values}
#'   in the document-feature matrix.  To select subsets of a dfm based on the
#'   features themselves (meaning the feature labels from
#'   \code{\link{featnames}}) -- such as those matching a regular expression, or
#'   removing features matching a stopword list, use \code{\link{dfm_select}}.
#' @seealso \code{\link{dfm_select}}, \code{\link{dfm_sample}}
#' @examples
#' (myDfm <- dfm(data_corpus_inaugural[1:5]))
#'
#' # keep only words occurring >=10 times and in >=2 documents
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 2)
#'
#' # keep only words occurring >=10 times and in at least 0.4 of the documents
#' dfm_trim(myDfm, min_count = 10, min_docfreq = 0.4)
#'
#' # keep only words occurring <=10 times and in <=2 documents
#' dfm_trim(myDfm, max_count = 10, max_docfreq = 2)
#'
#' # keep only words occurring <=10 times and in at most 3/4 of the documents
#' dfm_trim(myDfm, max_count = 10, max_docfreq = 0.75)
#'
#' # keep only words occurring frequently (top 20%) and in <=2 documents
#' dfm_trim(myDfm, min_count = 0.8, max_docfreq = 2)
#'
#' # keep only words occurring 5 times in 1000, and in 2 of 5 of documents
#' dfm_trim(myDfm, min_docfreq = 0.4, min_count = 0.005)
#'
#' \dontrun{
#' # compare to removeSparseTerms from the tm package
#' (myDfmTM <- convert(myDfm, "tm"))
#' tm::removeSparseTerms(myDfmTM, 0.7)
#' dfm_trim(myDfm, min_docfreq = 0.3)
#' dfm_trim(myDfm, sparsity = 0.7)
#' }
#' 
#' @export
dfm_trim <- function(x, min_count = 1, min_docfreq = 1, 
                     max_count = NULL, max_docfreq = NULL, 
                     sparsity = NULL, 
                     verbose = quanteda_options("verbose")) {
    UseMethod("dfm_trim")
}

#' @export
dfm_trim.default <- function(x, min_count = 1, min_docfreq = 1, 
                             max_count = NULL, max_docfreq = NULL, 
                             sparsity = NULL, 
                             verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "dfm_trim"))
}
    
#' @export
dfm_trim.dfm <- function(x, min_count = 1, min_docfreq = 1, 
                         max_count = NULL, max_docfreq = NULL, 
                         sparsity = NULL, 
                         verbose = quanteda_options("verbose")) {
    
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    
    # warning if already fractional
    if (missing(min_count) && 
        (x@weightTf$scheme != "count" | x@weightDf$scheme != "unary")) {
        warning("dfm has been previously weighted; consider changing default min_count")
    }
    
    freq <- colSums(x)
    freq_doc <- docfreq(x)
    
    if (!is.null(sparsity)) {
        if ((min_docfreq > 1 || !is.null(max_docfreq)) && !is.null(sparsity))
            stop("min/max_docfreq and sparsity both refer to a document ",
                 "threshold, both should not be specified")
        if (verbose) 
            catm("Note: converting sparsity into min_docfreq = 1 -", 
                 sparsity, "=", format(min_docfreq, big.mark=","), ".\n")
        min_docfreq <- 1.0 - sparsity
    }             

    # convert fractions into counts
    if (min_count < 1.0)
        min_count <- quantile(freq, min_count, names = FALSE, type = 1)
 
    if (!is.null(max_count) && max_count < 1.0)
        max_count <- quantile(freq, max_count, names = FALSE, type = 1)
    
    if (min_docfreq < 1.0)
        min_docfreq <- min_docfreq * ndoc(x)
    
    if (!is.null(max_docfreq) && max_docfreq < 1.0)
        max_docfreq <- max_docfreq * ndoc(x)

    # set maximums appropriately if not provided
    if (is.null(max_count))
        max_count <- max(freq, min_count)
    if (is.null(max_docfreq))
        max_docfreq <- max(freq_doc, min_docfreq)
    
    # checks that min is less than max
    if (max_count < min_count)
        stop("max_count must be >= min_count")
    if (max_docfreq < min_docfreq)
        stop("max_docfreq must be >= min_docfreq")
    
    flag_min_count <- freq < min_count
    flag_max_count <- freq > max_count
    flag_min_doc <- freq_doc < min_docfreq
    flag_max_doc <- freq_doc > max_docfreq
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
            catm("  - fewer than ", min_count, " time",
                 if (min_count != 1L) "s" else "", ": ", 
                 format(sum(flag_min_count), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_count)) {
            catm("  - more than ", max_count, " time",
                 if (max_count != 1L) "s" else "", ": ", 
                 format(sum(flag_max_count), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
        
    # print messages about docfreq removal
    if (verbose && (sum(flag_min_doc) || sum(flag_max_doc))) {
        if (sum(flag_min_doc)) {
            catm("  - in fewer than ", min_docfreq, " document", 
                 ifelse(min_count != 1, "s", ""), ": ", 
                 format(sum(flag_min_doc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
        if (sum(flag_max_doc)) {
            catm("  - in more than ", max_docfreq, " document", 
                 ifelse(max_docfreq != 1, "s", ""), ": ", 
                 format(sum(flag_max_doc), big.mark = ","), 
                 sep = "", appendLF = TRUE)
        }
    }
    
    if (verbose) {
        catm("  Total features removed: ", format(sum(flag_all), big.mark=","), 
             " (", 
             format(sum(flag_all) / nfeat(x) * 100, digits = 3, nsmall = 1), 
             "%).", 
             sep = "", appendLF = TRUE)
    }
       
    x[, !flag_all]
}
