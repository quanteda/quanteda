#' calculate collocation statistics
#' 
#' Identify and score collocations from a corpus, character, or tokens object, 
#' with targeted selection.
#' @param x a character, \link{corpus}, or \link{tokens} object to be mined for 
#'   collocations
#' @param method association measure for detecting collocations.  Let \eqn{i} 
#'   index documents, and \eqn{j} index features, \eqn{n_{ij}} refers to 
#'   observed counts, and \eqn{m_{ij}} the expected counts in a collocations 
#'   frequency table of dimensions \eqn{(J - size + 1)^2}. Available measures 
#'   are computed as: \describe{ \item{\code{"lr"}}{The likelihood ratio 
#'   statistic \eqn{G^2}, computed as: \deqn{2 * \sum_i \sum_j ( n_{ij} * log 
#'   \frac{n_{ij}}{m_{ij}} )} } \item{\code{"chi2"}}{Pearson's \eqn{\chi^2} 
#'   statistic, computed as: \deqn{\sum_i \sum_j \frac{(n_{ij} - 
#'   m_{ij})^2}{m_{ij}}} } \item{\code{"pmi"}}{point-wise mutual information 
#'   score, computed as log \eqn{n_{11}/m_{11}}} \item{\code{"dice"}}{the Dice 
#'   coefficient, computed as \eqn{n_{11}/n_{1.} + n_{.1}}} 
#'   \item{\code{"bj_uni"}}{unigram subtuples, Blaheta and Johnson's method (called through 
#'   \code{\link{sequences}})}  \item{\code{"bj_all"}}{all subtuples algorithm, 
#'   Blaheta and Johnson's method (called through \code{\link{sequences}})} }
#' @param size numeric argument representing the length of the collocations
#'   to be scored.  The maximum size is currently 3 for all
#'   methods except \code{"bj_uni"} and \code{"bj_all"}, which has a maximum size of 5.
#'   Use c(2,n) or 2:n to return collocations of bigram to n-gram collocations.
#' @param min_count minimum frequency of collocations that will be scored
#' @param ... additional arguments passed to \code{\link{collocations2}} for the
#'   first four methods, or to  \code{\link{sequences}} for \code{method = "bj"}
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#'   
#'   McInnes, B T. 2004. "Extending the Log Likelihood Measure to Improve 
#'   Collocation Identification."  M.Sc. Thesis, University of Minnesota.
#' @note 
#' This function is under active development, and we aim to improve both its operation and 
#' efficiency in the next release of \pkg{quanteda}.
#' @export
#' @keywords textstat collocations experimental
#' @examples
#' txts <- c("quanteda is a package for quantitative text analysis", 
#'           "quantitative text analysis is a rapidly growing field", 
#'           "The population is rapidly growing")
#' toks <- tokens(txts)
#' textstat_collocations(toks, method = "lr")
#' textstat_collocations(toks, method = "lr", min_count = 1)
#' textstat_collocations(toks, method = "lr", size = 2:3, min_count = 1)
#' (cols <- textstat_collocations(toks, method = "lr", size = 2:3, min_count = 2))
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' toks2 <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks2 <- tokens_select(toks2, stopwords("english"), "remove", padding = TRUE)
#' toks2 <- tokens_select(toks2, "^([A-Z][a-z\\-]{2,})", valuetype="regex", 
#'                      case_insensitive = FALSE, padding = TRUE)
#' seqs <- textstat_collocations(toks2, method = "bj_uni")
#' head(seqs, 10)
#' 
#' # compounding tokens is more efficient when applied to the same tokens object 
#' toks_comp <- tokens_compound(toks2, seqs)
textstat_collocations <- function(x, method =  c("lr", "chi2", "pmi", "dice", "bj_uni", "bj_all"), 
                                  size = 2,
                                  min_count = 2, 
                                  ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, method =  c("lr", "chi2", "pmi", "dice", "bj_uni", "bj_all"), 
                                         size = 2,
                                         min_count = 2, 
                                         ...) {
    method <- match.arg(method)
    if (method == 'bj_uni') {
        result <- sequences(x, min_count = min_count, size = size, ...)
    } else if (method == 'bj_all'){
        result <- sequences(x, min_count = min_count, size = size, method = "all_subtuples", ...)
    } else {
        if (!all(size %in% 2:3)) {
            stop("for method ", method, " size can only be 2, 3, or 2:3")
        } 
        
        result <- collocations2(x, method = method,  
                                size = size, min_count = min_count, ...)
    }
    rownames(result) <- seq_len(nrow(result))
    class(result) <- c("collocations", 'data.frame')
    return(result)
}

#' 
#' #' @rdname textstat_collocations
#' #' @noRd
#' #' @export    
#' textstat_collocations.corpus <- function(x, method =  c("lr", "chi2", "pmi", "dice", "bj"), ...) {
#'     method <- match.arg(method)
#'     textstat_collocations(texts(x), method = method, ...)
#' }
#' 
#' #' @rdname textstat_collocations
#' #' @noRd
#' #' @export    
#' textstat_collocations.character <- function(x, method = c("lr", "chi2", "pmi", "dice", "bj"), ...) {
#'     method <- match.arg(method)
#'     textstat_collocations(tokens(x), method = method, ...)
#' }
#' 
#' #' @rdname textstat_collocations
#' #' @noRd
#' #' @export    
#' textstat_collocations.tokenizedTexts <- function(x, method = c("lr", "chi2", "pmi", "dice", "bj"), ...) {
#'     method <- match.arg(method)
#'     textstat_collocations(as.tokens(x), method = method, ...)
#' }
#' 
#

#' check if an object is collocations object
#' @rdname textstat_collocations
#' @aliases is.collocations
#' @export
#' @return \code{is.collocation} returns \code{TRUE} if the object is of class
#'   \code{collocations}, \code{FALSE} otherwise.
is.collocations <- function(x) {
    "collocations" %in% class(x)
}

#' @method "[" collocations
#' @export
#' @noRd
"[.collocations" <- function(x, i = TRUE, j = TRUE, ...) {
    toks <- attr(x, 'tokens')
    x <- as.data.frame(x)[i, j, ...]
    attr(x, 'tokens') <- toks[i]
    class(x) <- c("collocations", 'data.frame')
    return(x)
}



