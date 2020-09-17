#' Identify and score multi-word expressions
#' 
#' Identify and score multi-word expressions, or adjacent fixed-length collocations, from text.  
#' 
#' Documents are grouped for the purposes of scoring, but collocations will not span sentences.
#' If `x` is a [tokens] object and some tokens have been removed, this should be done
#' using `[tokens_remove](x, pattern, padding = TRUE)` so that counts will still be
#' accurate, but the pads will prevent those collocations from being scored.
#' @param x a character, [corpus], or [tokens] object whose
#'   collocations will be scored.  The tokens object should include punctuation,
#'   and if any words have been removed, these should have been removed with
#'   `padding = TRUE`.  While identifying collocations for tokens objects is 
#'   supported, you will get better results with character or corpus objects due
#'   to relatively imperfect detection of sentence boundaries from texts already 
#'   tokenized.
#' @param method association measure for detecting collocations. Currently this 
#' is limited to `"lambda"`.  See Details.
#' @param size integer; the length of the collocations
#'   to be scored
#' @param min_count numeric; minimum frequency of collocations that will be scored
#' @param smoothing numeric; a smoothing parameter added to the observed counts
#'   (default is 0.5)
#' @param tolower logical; if `TRUE`, form collocations as lower-cased combinations
#' @param ... additional arguments passed to [tokens()], if `x`
#'   is not a [tokens] object already
#' @references Blaheta, D. & Johnson, M. (2001). 
#'   [Unsupervised
#'   learning of multi-word verbs](http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf). Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @note 
#' This function is under active development, with more measures to be added in the 
#' the next release of \pkg{quanteda}.
#' @details 
#' The `lambda` computed for a size = \eqn{K}-word target multi-word
#' expression the coefficient for the  \eqn{K}-way interaction parameter in the
#' saturated log-linear model fitted to the counts of the terms forming the set
#' of eligible multi-word expressions. This is the same as the "lambda" computed
#' in Blaheta and Johnson's (2001), where all multi-word expressions are
#' considered (rather than just verbs, as in that paper). The `z` is the 
#' Wald \eqn{z}-statistic computed as the quotient of `lambda` and the Wald
#' statistic for `lambda` as described below.
#' 
#' In detail:
#' 
#' Consider a \eqn{K}-word target expression \eqn{x}, and let \eqn{z} be any
#' \eqn{K}-word expression. Define a comparison function \eqn{c(x,z)=(j_{1},
#' \dots, j_{K})=c} such that the \eqn{k}th element of \eqn{c} is 1 if the
#' \eqn{k}th word in \eqn{z} is equal to the \eqn{k}th word in \eqn{x}, and 0
#' otherwise. Let \eqn{c_{i}=(j_{i1}, \dots, j_{iK})}, \eqn{i=1, \dots,
#' 2^{K}=M}, be the possible values of \eqn{c(x,z)}, with \eqn{c_{M}=(1,1,
#' \dots, 1)}. Consider the set of \eqn{c(x,z_{r})} across all expressions
#' \eqn{z_{r}} in a corpus of text, and let \eqn{n_{i}}, for \eqn{i=1,\dots,M},
#' denote the number of the \eqn{c(x,z_{r})} which equal \eqn{c_{i}}, plus the
#' smoothing constant `smoothing`. The \eqn{n_{i}} are the counts in a
#' \eqn{2^{K}} contingency table whose dimensions are defined by the
#' \eqn{c_{i}}.
#' 
#' \eqn{\lambda}: The \eqn{K}-way interaction parameter in the saturated
#' loglinear model fitted to the \eqn{n_{i}}. It can be calculated as
#' 
#' \deqn{\lambda  = \sum_{i=1}^{M} (-1)^{K-b_{i}} * log n_{i}}
#' 
#' where \eqn{b_{i}} is the number of the elements of \eqn{c_{i}} which are
#' equal to 1.
#' 
#' Wald test \eqn{z}-statistic \eqn{z} is calculated as:
#' 
#' \deqn{z = \frac{\lambda}{[\sum_{i=1}^{M} n_{i}^{-1}]^{(1/2)}}}
#'
#' @return `textstat_collocations` returns a data.frame of collocations and
#'   their scores and statistics. This consists of the collocations, their
#'   counts, length, and \eqn{\lambda} and \eqn{z} statistics.  When `size`
#'   is a vector, then `count_nested` counts the lower-order collocations
#'   that occur within a higher-order collocation (but this does not affect the
#'   statistics).
#' @export
#' @keywords textstat collocations
#' @aliases collocations
#' @author Kenneth Benoit, Jouni Kuha, Haiyan Wang, and Kohei Watanabe
#' @examples
#' corp <- data_corpus_inaugural[1:2]
#' head(cols <- textstat_collocations(corp, size = 2, min_count = 2), 10)
#' head(cols <- textstat_collocations(corp, size = 3, min_count = 2), 10)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' toks1 <- tokens(data_corpus_inaugural)
#' toks2 <- tokens_remove(toks1, pattern = stopwords("english"), padding = TRUE)
#' toks3 <- tokens_select(toks2, pattern = "^([A-Z][a-z\\-]{2,})", valuetype = "regex",
#'                        case_insensitive = FALSE, padding = TRUE)
#' tstat <- textstat_collocations(toks3, size = 3, tolower = FALSE)
#' head(tstat, 10)
#' 
#' # vectorized size
#' txt <- c(". . . . a b c . . a b c . . . c d e",
#'          "a b . . a b . . a b . . a b . a b",
#'          "b c d . . b c . b c . . . b c")
#' textstat_collocations(txt, size = 2:3)
#' 
textstat_collocations <- function(x, method = "lambda", 
                                  size = 2, 
                                  min_count = 2, 
                                  smoothing = 0.5, 
                                  tolower = TRUE, 
                                  ...) { 
    UseMethod("textstat_collocations")
}

#' @export
textstat_collocations.default <- function(x, method = "lambda", 
                                         size = 2, 
                                         min_count = 2, 
                                         smoothing = 0.5, 
                                         tolower = TRUE, 
                                         ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_collocations"))
}
    

#' @importFrom stats na.omit
#' @export
textstat_collocations.tokens <- function(x, method = "lambda", 
                                         size = 2, 
                                         min_count = 2, 
                                         smoothing = 0.5, 
                                         tolower = TRUE, 
                                         ...) {
    x <- as.tokens(x)
    method <- match.arg(method, c("lambda"))
    
    if (any(size == 1))
        stop("Collocation sizes must be larger than 1")
    if (any(size > 5))
        warning("Computation for large collocations may take long time", immediate. = TRUE)
    
    # lower case if requested
    if (tolower) x <- tokens_tolower(x, keep_acronyms = TRUE)
    
    attrs <- attributes(x)
    types <- types(x)
    id_ignore <- unlist(pattern2id("^\\p{P}+$", types, 'regex', FALSE), use.names = FALSE)
    if (is.null(id_ignore)) id_ignore <- integer()
    result <- qatd_cpp_collocations(x, types, id_ignore, min_count, size, 
                                    if (method == "lambda1") "lambda1" else "lambda", 
                                    smoothing)
    
    # compute z for lambda methods
    result$z <- result$lambda / result$sigma
    result$sigma <- NULL
    # result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, result$collocation, 
                           decreasing = c(TRUE, FALSE), method = "radix"), ]

    # remove results whose counts are less than min_count
    result <- result[result$count >= min_count, ]
    # tag attributes and class, and return
    attr(result, 'types') <- types
    class(result) <- c('collocations', 'textstat', 'data.frame')
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}


#' @export
textstat_collocations.corpus <- function(x, method = "lambda", 
                                         size = 2, 
                                         min_count = 2, 
                                         smoothing = 0.5, 
                                         tolower = TRUE, 
                                         recursive = TRUE, ...) {
    x <- as.corpus(x)
    textstat_collocations(tokens(x, ...), method = method, size = size, 
                          min_count = min_count, 
                          smoothing = smoothing, tolower = tolower)
}

#' @export
textstat_collocations.character <- function(x, method = "lambda", 
                                            size = 2, 
                                            min_count = 2, 
                                            smoothing = 0.5, 
                                            tolower = TRUE, 
                                            recursive = TRUE, ...) {
    textstat_collocations(corpus(x), method = method, size = size, 
                          min_count = min_count, 
                          smoothing = smoothing, tolower = tolower, ...)
}


#' @rdname textstat_collocations
#' @export
#' @return `is.collocation` returns `TRUE` if the object is of class
#'   `collocations`, `FALSE` otherwise.
is.collocations <- function(x) {
    "collocations" %in% class(x)
}

# Internal Functions ------------------------------------------------------

# function to get lower-order interactions for k-grams
# example:
#  marginalfun(3)
#  ## [1]]
#  ## [1] 1 2
#  ## 
#  ## [[2]]
#  ## [1] 1 3
#  ## 
#  ## [[3]]
#  ## [1] 2 3
marginalfun <- function(k) {
    utils::combn(k, k-1, simplify = FALSE)
}

# create sorted nxxx combinations where x is in {0,1}
# example:
#  make_count_names(3, "n")
#  ## [1] "n000" "n001" "n010" "n011" "n100" "n101" "n110" "n111"
#  make_count_names(2, "e")
#  ## [1] "e00" "e01" "e10" "e11"
make_count_names <- function(size, label = c("n", "e")) {
    combinations <- cbind(label, expand.grid(rep(list(0:1), size)))
    combinations <- apply(combinations, 1, paste, collapse = "")
    sort(combinations)
}

# function to get expected counts from IPF
get_expected_values <- function(df, size) {
    # get the columns of the data.frame that are the n* counts
    counts <- df[, grep("^n\\d+", names(df))]
    # sort the counts alphabetically
    counts <- df[, sort(names(counts))]
    
    expected_counts_list <- apply(counts, 1, function(x) {
        countsnum <- as.numeric(x)
        names(countsnum) <- names(counts)
        array_dimnames <- c(rep(list(c("0", "1")), size))
        names(array_dimnames) <- paste0("W", size:1)
        counts_table <- array(countsnum, dim = rep(2, size), 
                              dimnames = array_dimnames)
        counts_expected <- stats::loglin(counts_table,
                                         margin =  marginalfun(size),
                                         fit = TRUE, print = FALSE)$fit
        counts_expected <- as.numeric(counts_expected)
        names(counts_expected) <- gsub("e", "n", names(counts))
        counts_expected
    })
    
    data.frame(t(expected_counts_list))
}
