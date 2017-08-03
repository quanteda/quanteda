#' identify and score multi-word expressions
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
#'   \code{padding = TRUE}
#' @param method association measure for detecting collocations. Let \eqn{i}
#'   index documents, and \eqn{j} index features, \eqn{n_{ij}} refers to 
#'   observed counts, and \eqn{m_{ij}} the expected counts in a collocations 
#'   frequency table of dimensions \eqn{(J - size + 1)^2}.
#'   
#'   Available measures are computed as: 
#'   
#'   \describe{
#'   \item{\code{"lambda"}}{log-linear coefficient for the full-ngram, similar to Blaheta and Johnson's (2001) method}
#'   \item{\code{"lambda1"}}{unigram subtuples, alternative score from Blaheta and Johnson (2001)}
#'   \item{\code{"lr"}}{The likelihood ratio statistic \eqn{G^2}, computed as: \deqn{2 * \sum_i \sum_j ( n_{ij} * log 
#'   \frac{n_{ij}}{m_{ij}} )} } 
#'   \item{\code{"chi2"}}{Pearson's \eqn{\chi^2} statistic, computed as: 
#'   \deqn{\sum_i \sum_j \frac{(n_{ij} - m_{ij})^2}{m_{ij}}} } 
#'   \item{\code{"pmi"}}{point-wise mutual information 
#'   score, computed as log \eqn{n_{11}/m_{11}}} 
#'   \item{\code{"dice"}}{the Dice coefficient, computed as \eqn{n_{11}/n_{1.} + n_{.1}}} 
#   \item{\code{"gensim"}}{gensim score, coumputed as \eqn{(cnt(a, b) - min_count) * N / (cnt(a) * cnt(b))}}   
#   \item{\code{"LFMD"}}{LFMD, computed as \eqn{log2(P(w1,w2)^2/P(w1)P(w2)) + log2(P(w1,w2))}}
#'    }
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
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#'   
#'   McInnes, B T. (2004). "Extending the Log Likelihood Measure to Improve 
#'   Collocation Identification."  M.Sc. Thesis, University of Minnesota.
#'   
#'   A. Thanopoulos et al(2002), 
#'   \href{http://www.lrec-conf.org/proceedings/lrec2002/pdf/128.pdf}{Comparative evaluation of collocations extraction metrics}
#'   
#'   gensim score,  
#'   \href{https://radimrehurek.com/gensim/models/phrases.html#gensim.models.phrases.Phrases}{gensim models}
#' @note 
#' This function is under active development, and we aim to improve both its operation and 
#' efficiency in the next release of \pkg{quanteda}.
#' @return \code{textstat_collocations} returns a data.frame of collocations and their
#'   scores and statistsics.
#' @export
#' @keywords textstat collocations experimental
#' @examples
#' txts <- data_corpus_inaugural[1:2]
#' head(cols <- textstat_collocations(txts, size = 2, min_count = 2), 10)
#' head(cols <- textstat_collocations(txts, size = 3, min_count = 2), 10)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' toks2 <- tokens(data_corpus_inaugural)
#' toks2 <- tokens_remove(toks2, stopwords("english"), padding = TRUE)
#' toks2 <- tokens_select(toks2, "^([A-Z][a-z\\-]{2,})", valuetype = "regex", 
#'                        case_insensitive = FALSE, padding = TRUE)
#' seqs <- textstat_collocations(toks2, size = 3, tolower = FALSE)
#' head(seqs, 10)
textstat_collocations <- function(x, method = "all", size = 2, min_count = 2, smoothing = 0.5,  tolower = TRUE, show_counts = FALSE, ...) {
    UseMethod("textstat_collocations")
}

VALID_SCORING_METHODS <- c("lambda", "lambda1", "lr", "chi2", "pmi") #, "dice", "gensim", "LFMD")

#' @noRd
#' @export
#' @importFrom stats na.omit
textstat_collocations.tokens <- function(x, method = "all", size = 2, min_count = 1, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {

    method <- match.arg(method, c("all", VALID_SCORING_METHODS))
    if (any(!(size %in% 2:5)))
        stop("Only bigram, trigram, 4-gram and 5-gram collocations implemented so far.")

    # lower case if requested
    if (tolower) x <- tokens_tolower(x, keep_acronyms = TRUE)

    attrs_org <- attributes(x)
    types <- types(x)
    result <- qatd_cpp_sequences(x, types, min_count, size, 
                                 if (method == "lambda1") "lambda1" else "lambda", 
                                 smoothing)
    
    # compute z for lambda methods
    lambda_index <- which(stri_startswith_fixed(names(result), "lambda"))
    result["z"] <- result[lambda_index] / result[["sigma"]]
    # result$p <- 1 - stats::pnorm(result$z)
    
    # remove gensim and LFMD for now
    result[c("gensim", "LFMD", "dice")] <- NULL
    
    # sort by decreasing z
    result <- result[order(result[["z"]], decreasing = TRUE), ]

    # compute statistics that require expected counts
    if (method %in% c("all", "lr", "chi2", "pmi") | show_counts) {
        # get observed counts and compute expected counts
        # split the string into n00, n01, n10, etc
        counts_n <- strsplit(result[, "observed_counts"], "_")
        df_counts_n <- data.frame(t(sapply(counts_n, as.numeric)))
        names(df_counts_n) <- make_count_names(size, "n")
        # compute expected counts
        df_counts_e <- get_expected_values(df_counts_n, size = size)
        names(df_counts_e) <- make_count_names(size, "e")
        # remove observed counts character
        result <- result[, -which(names(result)=="observed_counts")]
        
        # "pmi_2", "chi2_2" and "G2_2" are verified, remove the result from cpp
        result[c("pmi", "chi2", "G2")] <- NULL
        
        # recompute dice, pmi, G2, chi2
        if (method %in% c("all", "lr"))
            result["G2"] <- 2 * rowSums(df_counts_n * log(df_counts_n / df_counts_e))
        if (method %in% c("all", "chi2"))
            result["chi2"] <- rowSums((df_counts_n - df_counts_e)^2 / df_counts_e)
        if (method %in% c("all", "pmi"))
            result["pmi"] <- log(df_counts_n[[ncol(df_counts_n)]] / df_counts_e[[ncol(df_counts_e)]], base = 2)
    }

    # remove other measures if not specified
    if (method == "lambda" | method == "lambda1")
        result[c("pmi", "chi2", "G2")] <- NULL
    if (!method %in% c("lambda", "lambda1", "all"))
        result[c("lambda", "lambda1", "sigma", "z")] <- NULL
    #if (method == "chi2") result[c("pmi", "G2")] <- NULL
    #if (method == "lr") result[c("pmi", "chi2")] <- NULL
    #if (method == "pmi") result[c("G2", "chi2")] <- NULL
    
    # remove results whose counts are less than min_count
    result <- result[result$count >= min_count, ]
    
    # reorder columns
    result <- result[, stats::na.omit(match(c("collocation", "count", "length", "lambda", "lambda1", "sigma", "z", 
                                       "G2", "G2_2", "chi2", "chi2_2", "pmi", "pmi_2"), 
                                     names(result)))]
    
    # add counts to output if requested
    if (show_counts) result <- cbind(result, df_counts_n, df_counts_e)

    # tag attributes and class, and return
    attr(result, 'types') <- types
    rownames(result) <- seq_len(nrow(result))
    class(result) <- c("collocations", 'data.frame')
    return(result)
}


#' @export
textstat_collocations.corpus <- function(x, method = "all", size = 2, min_count = 1, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    # segment into units not including punctuation, to avoid identifying collocations that are not adjacent
    texts(x) <- paste(".", texts(x))
    # separate each line except those where the punctuation is a hyphen or apostrophe
    x <- corpus_segment(x, "tag", delimiter =  "[^\\P{P}#@'-]", valuetype = "regex")
    # tokenize the texts
    x <- tokens(x, ...)
    textstat_collocations(x, method = method, size = size, min_count = min_count, smoothing = smoothing, tolower = tolower, show_counts = show_counts)
}

#' @export
textstat_collocations.character <- function(x, method = "all", size = 2, min_count = 1, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    textstat_collocations(corpus(x), method = method, size = size, min_count = min_count, 
                          smoothing = smoothing, tolower = tolower, show_counts = show_counts, ...)
}

#' @export
textstat_collocations.tokenizedTexts <- function(x, method = "all", size = 2, min_count = 1, smoothing = 0.5, tolower = TRUE, show_counts = FALSE, ...) {
    textstat_collocations(as.tokens(x), method = method, size = size, min_count = min_count, 
                          smoothing = smoothing, tolower = tolower, show_counts = FALSE)
}


#' @rdname textstat_collocations
#' @aliases is.collocations
#' @export
#' @return \code{is.collocation} returns \code{TRUE} if the object is of class
#'   \code{collocations}, \code{FALSE} otherwise.
is.collocations <- function(x) {
    "collocations" %in% class(x)
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
        counts_table <- array(countsnum, dim = rep(2, size), dimnames = array_dimnames)
        counts_expected <- stats::loglin(counts_table,
                                         margin =  marginalfun(size),
                                         fit = TRUE, print = FALSE)$fit
        counts_expected <- as.numeric(counts_expected)
        names(counts_expected) <- gsub("e", "n", names(counts))
        counts_expected
    })
    
    data.frame(t(expected_counts_list))
}

