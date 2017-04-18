# @param target the document index (numeric, character or logical) identifying the 
#   document forming the "target" for computing keyness; all other documents' 
#   feature frequencies will be combined for use as a reference
# @param measure (signed) association measure to be used for computing keyness.
#   Currenly available: \code{"chi2"} (\eqn{chi^2} with Yates correction); 
#   \code{"exact"} (Fisher's exact test); \code{"lr"} for the likelihood ratio
#   \eqn{G} statistic with Yates correction.


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
#'   \item{\code{"bj"}}{Blaheta and Johnson's method (called through 
#'   \code{\link{sequences}})} }
#' @param ... additional arguments passed to \code{\link{collocations}} for the first four
#'    methods, or to  \code{\link{sequences}} for \code{method = "bj"}
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations. 
#'   
#'   McInnes, B T. 2004. "Extending the Log Likelihood Measure to Improve
#'   Collocation Identification."  M.Sc. Thesis, University of Minnesota.
#' @export
#' @keywords textstat collocations experimental
#' @examples
#' \dontrun{
#' txts <- c("This is software testing: looking for (word) pairs!  
#'            This [is] a software testing again. For.",
#'           "Here: this is more Software Testing, looking again for word pairs.")
#' toks <- tokens(txts)
#' textstat_collocations(toks, method = "lr")
#' textstat_collocations(toks, method = "lr", size = 3, min_count = 2)
#' textstat_collocations(toks, method = "lr", size = 3, min_count = 1)
#' (cols <- textstat_collocations(toks, method = "lr", "^([a-z]+)$", 
#'                                valuetype = "regex", min_count = 1))
#' as.tokens(cols)
#' 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' seqs <- textstat_collocations(toks, method = "bj", 
#'                               features = "^([A-Z][a-z\\-]{2,})", 
#'                               valuetype = "regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # more efficient when applied to the same tokens object 
#' toks_comp <- tokens_compound(toks, seqs)
#' 
#' # types can be any words
#' seqs2 <- textstat_collocations(toks, "bj", "^([a-z]+)$", valuetype="regex", 
#'                                case_insensitive = FALSE, min_count = 2, ordered = TRUE)
#' head(seqs2, 10)
#' 
#' }
textstat_collocations <- function(x, method =  c("lr", "chi2", "pmi", "dice", "bj"), ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, method =  c("lr", "chi2", "pmi", "dice", "bj"), ...) {
    method <- match.arg(method)
    if (method == 'bj') {
        result <- sequences(x, ...)
    } else {
        result <- quanteda:::collocations2(x, method = method, ...)
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


#' @rdname collocations
#' @export
#' @return \code{is.collocation} returns \code{TRUE} if the object is of class
#'   collocations, \code{FALSE} otherwise.
is.collocations <- function(x) {
    "collocations" %in% class(x)
}


#' #' @method "[" collocations
#' #' @export
#' #' @noRd
#' "[.collocations" <- function(x, i, ...) {
#'     x <- as.data.frame(x)[i,]
#'     attr(x, 'ids') <- attr(x, 'ids')[i]
#'     class(x) <- c("collocations", 'data.frame')
#'     return(x)
#' }
#' 
#' @export
#' @method as.tokens collocations
#' @noRd
as.tokens.collocations <- function(x) {
    toks <- attr(x, 'tokens')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}


