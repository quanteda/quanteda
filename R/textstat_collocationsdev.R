#' Identify and score multi-word expressions
#' 
#' Identify and score multi-word expressions, or adjacent fixed-length collocations, from text.  
#' 
#' Documents are grouped for the purposes of scoring, but collocations will not span sentences.
#' If \code{x} is a \link{tokens} object and some tokens have been removed, this should be done
#' using \code{\link{tokens_remove}(x, pattern, padding = TRUE)} so that counts will still be
#' accurate, but the pads will prevent those collocations from being scored.
#' @param x a character, \link{corpus}, or \link{tokens} object whose
#'   collocations will be scored.  The tokens object should include punctuation,
#'   and if any words have been removed, these should have been removed with
#'   \code{padding = TRUE}.  While identifying collocations for tokens objects is 
#'   supported, you will get better results with character or corpus objects due
#'   to relatively imperfect detection of sentence boundaries from texts already 
#'   tokenized.
#' @param method association measure for detecting collocations: \code{"all"},
#'   \code{"lambda"}, \code{"lambda1"}, \code{"lr"}, \code{"chi2"}, and
#'   \code{"dice"}.  See Details.
#' @param size integer; the length of the collocations
#'   to be scored
#' @param min_count numeric; minimum frequency of collocations that will be scored
#' @param smoothing numeric; a smoothing parameter added to the observed counts
#'   (default is 0.5)
#' @param tolower logical; if \code{TRUE}, form collocations as lower-cased combinations
#' @param show_counts logical; if \code{TRUE}, output observed and expected counts
#' @param ... additional arguments passed to \code{\link{tokens}}, if \code{x}
#'   is not a \link{tokens} object already
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'   learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @note 
#' This function is under active development, with more measures to be added in the 
#' the next release of \pkg{quanteda}.
#' @details 
#' The \code{lambda} computed for a size = \eqn{K}-word target multi-word
#' expression the coefficient for the  \eqn{K}-way interaction parameter in the
#' saturated log-linear model fitted to the counts of the terms forming the set
#' of eligible multi-word expressions. This is the same as the "lambda" computed
#' in Blaheta and Johnson's (2001), where all multi-word expressions are
#' considered (rather than just verbs, as in that paper). The \code{z} is the 
#' Wald \eqn{z}-statistic computed as the quotient of \code{lambda} and the Wald
#' statistic for \code{lambda} as described below.
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
#' smoothing constant \code{smoothing}. The \eqn{n_{i}} are the counts in a
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
#' @return \code{textstat_collocationsdev} returns a data.frame of collocations and their
#'   scores and statistics.
#' @export
#' @keywords textstat collocations experimental internal
#' @author Kenneth Benoit, Jouni Kuha, Haiyan Wang, and Kohei Watanabe
#' @examples
#' txts <- data_corpus_inaugural[1:2]
#' head(cols <- textstat_collocationsdev(txts, size = 2, min_count = 2), 10)
#' head(cols <- textstat_collocationsdev(txts, size = 3, min_count = 2), 10)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' toks2 <- tokens(data_corpus_inaugural)
#' toks2 <- tokens_remove(toks2, stopwords("english"), padding = TRUE)
#' toks2 <- tokens_select(toks2, "^([A-Z][a-z\\-]{2,})", valuetype = "regex", 
#'                        case_insensitive = FALSE, padding = TRUE)
#' seqs <- textstat_collocationsdev(toks2, size = 3, tolower = FALSE)
#' head(seqs, 10)
textstat_collocationsdev <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5,  tolower = TRUE, show_counts = FALSE, ...) {
    UseMethod("textstat_collocationsdev")
}

VALID_SCORING_METHODS <- c("lambda", "lambda1", "lr", "chi2", "pmi", "LFMD") #, "dice", "gensim")

#' @noRd
#' @export
#' @importFrom stats na.omit
textstat_collocationsdev.tokens <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    
    method <- match.arg(method, c("all", VALID_SCORING_METHODS))
    if (any(size == 1))
        stop("Collocation sizes must be larger than 1")
    if (any(size > 5))
        stop("Collocation sizes must be smaller than 6")
    
    if (length(size) > 1 & show_counts == TRUE)
        stop("show_counts only works when the size of the collocation is fixed")
    
    # lower case if requested
    if (tolower) x <- tokens_tolower(x, keep_acronyms = TRUE)
    
    attrs <- attributes(x)
    types <- types(x)
    id_ignore <- unlist(regex2id("^\\p{P}+$", types, 'regex', FALSE), use.names = FALSE)
    if (is.null(id_ignore)) id_ignore <- integer()
    
    result <- qatd_cpp_collocations_dev(x, types, min_count, size, method, smoothing) 
    
    # remove results whose counts are less than min_count
    result <- result[result$count >= min_count, ]
    
    # compute z for lambda methods
    if (method %in% c("lambda", "lambda1", "all")){
        
        result["z"] <- result[["method"]] / result[["sigma"]]
        # result$p <- 1 - stats::pnorm(result$z)
        if (method == "all"){
            colnames(result)[colnames(result) == "method"] <- "lambda"
        } else {
            colnames(result)[colnames(result) == "method"] <- method
        }
        
        # remove gensim and dice for now
        result[c("gensim", "dice", "sigma")] <- NULL
        
        # sort by decreasing z
        result <- result[order(result[["z"]], decreasing = TRUE), ]
    }
    
    if (method %in% c("all", "lr", "chi2", "pmi", "LFMD") & show_counts) {
        # get observed counts and compute expected counts
        # split the string into n00, n01, n10, etc
        counts_n <- strsplit(result[, "observed_counts"], "_")
        df_counts_n <- data.frame(t(vapply(counts_n, as.numeric, numeric(2^size))))
        names(df_counts_n) <- make_count_names(size, "n")
        # get expected counts
        counts_e <- strsplit(result[, "expected_counts"], "_")
        df_counts_e <- data.frame(t(vapply(counts_e, as.numeric, numeric(2^size))))
        names(df_counts_e) <- make_count_names(size, "e")
        # remove counts character
        result <- result[, -which(names(result)=="observed_counts")]
        result <- result[, -which(names(result)=="expected_counts")]
    }
    
    if (!show_counts){
        # remove counts character
        result <- result[, -which(names(result)=="observed_counts")]
        result <- result[, -which(names(result)=="expected_counts")]
    }
    # remove other measures if not specified
    if (method == "lambda" | method == "lambda1")
        result[c("pmi", "chi2", "G2", "sigma", "LFMD")] <- NULL
    if (!method %in% c("lambda", "lambda1", "all"))
        result[c("method", "sigma", "z")] <- NULL
    if (method == "chi2") result[c("pmi", "G2", "LFMD")] <- NULL
    if (method == "lr") result[c("pmi", "chi2", "LFMD")] <- NULL
    if (method == "pmi") result[c("G2", "chi2", "LFMD")] <- NULL
    if (method == "LFMD") result[c("G2", "chi2", "pmi")] <- NULL
    
    
    # reorder columns
    result <- result[, stats::na.omit(match(c("collocation", "count", "length", "lambda", "lambda1", "sigma", "z", 
                                              "G2", "chi2", "pmi", "LFMD"), 
                                            names(result)))]
    rownames(result) <- NULL
    # # add counts to output if requested
    if (show_counts) result <- cbind(result, df_counts_n, df_counts_e)
    
    
    # tag attributes and class, and return
    attr(result, 'types') <- types
    class(result) <- c("collocationsdev", 'data.frame')
    return(result)
}


#' @export
textstat_collocationsdev.corpus <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    # segment into units not including punctuation, to avoid identifying collocations that are not adjacent
    #texts(x) <- paste(".", texts(x))
    # separate each line except those where the punctuation is a hyphen or apostrophe
    #x <- corpus_segment(x, "tag", delimiter =  "[^\\P{P}#@'-]", valuetype = "regex")
    # tokenize the texts
    x <- tokens(x, ...)
    textstat_collocationsdev(x, method = method, size = size, min_count = min_count, smoothing = smoothing, tolower = tolower, show_counts = show_counts)
}

#' @export
textstat_collocationsdev.character <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    textstat_collocationsdev(corpus(x), method = method, size = size, min_count = min_count, 
                             smoothing = smoothing, tolower = tolower, show_counts = show_counts, ...)
}

#' @export
textstat_collocationsdev.tokenizedTexts <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    textstat_collocationsdev(as.tokens(x), method = method, size = size, min_count = min_count, 
                             smoothing = smoothing, tolower = tolower, show_counts = show_counts)
}


#' @rdname textstat_collocationsdev
#' @aliases is.collocationsdev
#' @export
#' @return \code{is.collocationdev} returns \code{TRUE} if the object is of class
#'   \code{collocationsdev}, \code{FALSE} otherwise.
is.collocationsdev <- function(x) {
    "collocationsdev" %in% class(x)
}

#  @method "[" collocations
#  @export
#  @noRd
# "[.collocations" <- function(x, i = TRUE, j = TRUE, ...) {
#     toks <- attr(x, 'tokens')
#     x <- as.data.frame(x)[i, j, ...]
#     attr(x, 'tokens') <- toks[i]
#     class(x) <- c("collocations", 'data.frame')
#     return(x)
# }

# returns TRUE if the object is of class sequences, FALSE otherwise
is.sequences <- function(x) "sequences" %in% class(x)

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

