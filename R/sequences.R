#' Find variable-length collocations 
#' 
#' This function automatically identifies contiguous collocations consisting of 
#' variable-length term sequences whose frequency is unlikely to have occurred 
#' by chance.  The algorithm is based on Blaheta and Johnson's (2001) 
#' "Unsupervised Learning of Multi-Word Verbs".
#' @param x a \link{tokens} object
#' @param min_count minimum frequency of sequences for which parameters are 
#'   estimated
#' @param size length of collocations, default is 2. Can be set up to 5.
#'        Use c(2,n) or 2:n to return collocations of bigram to n-gram collocations.
#' @param method default is "lambda" and option is "lambda1"
#' @param smoothing default is 0.5
#' @keywords collocations internal
#' @author Kohei Watanabe and Haiyan Wang
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @examples 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"), remove_punct=TRUE)
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' # extracting multi-part proper nouns (capitalized terms)
#' toks <- tokens_select(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", 
#'                      case_insensitive = FALSE, padding = TRUE)
#' 
#' seqs <- sequences(toks, size = 2:3)
#' head(seqs, 10)
#' # to return only trigrams
#' seqs <- sequences(toks, size=3)
#' head(seqs, 10)
#' @export
sequences <- function(x, 
                       min_count = 2,
                       size = 2,
                       method = c("lambda", "lambda1"),
                       smoothing = 0.5, 
                      show_counts = FALSE) {
    
    # .Deprecated('textstat_collocations')
    UseMethod("sequences")
}

#' @rdname sequences
#' @noRd
#' @export
sequences.tokens <- function(x,
                              min_count = 2,
                              size = 2,
                              method = c("lambda", "lambda1"),
                              smoothing = 0.5,
                             show_counts = FALSE) {
    
    attrs_org <- attributes(x)
    methodtype = match.arg(method)
    
    if (any(!(size %in% 2:5)))
        stop("Only bigram, trigram, 4-gram and 5-gram collocations implemented so far.")
    
    types <- types(x)
    
    result <- qatd_cpp_sequences(x, types, min_count, size, methodtype, smoothing)
    
    ###############output counts
    if (show_counts == TRUE){
        n_counts <- strsplit(result[, 12], "_")
        ttt <- sapply(n_counts, as.numeric)
        df_counts <- data.frame(t(ttt))
        if (size == 2){
            colnames(df_counts) <- c("n00", "n01", "n10", "n11")
        } else if (size == 3){
            colnames(df_counts) <- c("n000", "n001", "n010", "n011", "n100", "n101", "n110", "n111")
        } else if (size == 4){
            colnames(df_counts) <- c("n0000", "n0001", "n0010", "n0011", "n0100", "n0101", "n0110", "n1111", 
                                     "n1000", "n1001", "n1010", "n1011", "n1100", "n1101", "n1110", "n1111")
            
        }
        df_e_counts <- format(round(get_expected_values(df_counts, size = size), 1), nsmall = 1)

        if (size == 2){
            colnames(df_e_counts) <- c("e00", "e01", "e10", "e11")
        } else if (size == 3){
            colnames(df_e_counts) <- c("e000", "e001", "e010", "e011", "e100", "e101", "e110", "e111")
        } else if (size == 4){
            colnames(df_e_counts) <- c("e0000", "e0001", "e0010", "e0011", "e0100", "e0101", "e0110", "e1111", 
                                       "e1000", "e1001", "e1010", "e1011", "e1100", "e1101", "e1110", "e1111")
            
        }
        result <- cbind(result[, 1:11], df_counts, df_e_counts)
    } else {
        result <- result[, 1:11]
    }
    
    ######################
    result <- result[result$count >= min_count,]
    if (methodtype == "lambda") {
        result$z <- result$lambda / result$sigma
    } else {
        result$z <- result$lambda1 / result$sigma
    }
    result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, decreasing = TRUE),]
    attr(result, 'types') <- types
    class(result) <- c("sequences", 'data.frame')
    
    return(result)
}

#' @method "[" sequences
#' @export
#' @noRd
"[.sequences" <- function(x, i, ...) {
    x <- as.data.frame(x)[i,]
    attr(x, 'ids') <- attr(x, 'ids')[i]
    class(x) <- c("sequences", 'data.frame')
    return(x)
}

#' #' @export
#' #' @method as.tokens sequences
#' #' @noRd 
#' as.tokens.sequences <- function(x) {
#'     toks <- attr(x, 'tokens')
#'     attr(toks, 'types') <- attr(x, 'types')
#'     class(toks) <- c("tokens", "tokenizedTexts")
#'     return(toks)
#' }

#' @rdname sequences
#' @export
#' @return \code{sequences} returns \code{TRUE} if the object is of class
#'   sequences, \code{FALSE} otherwise.
is.sequences <- function(x) "sequences" %in% class(x)


# function to get lower-order interactions for k-grams
marginalfun <- function(k) {
    utils::combn(k, k-1, simplify = FALSE)
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