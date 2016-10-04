#' Create ngrams and skipgrams
#' 
#' Create a set of ngrams (tokens in sequence) from character vectors or
#' tokenized text objects, with an optional skip argument to form skipgrams. 
#' Both the ngram length and the skip lengths take vectors of arguments to form 
#' multiple lengths or skips in one pass.  \code{ngrams()} is implemented in C++
#' for efficiency.
#' @author Kohei Watanabe and Ken Benoit
#' @return a tokenizedTexts object consisting a list of character vectors of 
#'   ngrams, one list element per text, or a character vector if called on a 
#'   simple character vector
#' @param x a tokenizedText object or a character vector of tokens
#' @param n integer vector specifying the number of elements to be concatenated 
#'   in each ngram
#' @param skip integer vector specifying the adjacency skip size for tokens 
#'   forming the ngrams, default is 0 for only immediately neighbouring words. 
#'   For \code{skipgrams}, \code{skip} can be a vector of integers, as the 
#'   "classic" approach to forming skip-grams is to set skip = \eqn{k} where
#'   \eqn{k} is the distance for which \eqn{k} or fewer skips are used to construct
#'   the \eqn{n}-gram.  Thus a "4-skip-n-gram" defined as \code{skip = 0:4}
#'   produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0
#'   skips (where 0 skips are typical n-grams formed from adjacent words).  See
#'   Guthrie et al (2006).
#' @param concatenator character for combining words, default is \code{_} 
#'   (underscore) character
#' @param ... not used
#' @export
#' @examples
#' # ngrams
#' ngrams(LETTERS, n = 2)
#' ngrams(LETTERS, n = 2, skip = 1)
#' ngrams(LETTERS, n = 2, skip = 0:1)
#' ngrams(LETTERS, n = 1:2)
#' ngrams(LETTERS, n = c(2,3), skip = 0:1)
#' 
#' tokens <- tokenize("the quick brown fox jumped over the lazy dog.", 
#'                    removePunct = TRUE, simplify = TRUE)
#' ngrams(tokens, n = 1:3)
#' ngrams(tokens, n = c(2,4), concatenator = " ")
#' ngrams(tokens, n = c(2,4), skip = 1, concatenator = " ")
#' 
#' # skipgrams
ngrams <- function(x, ...) {
    UseMethod("ngrams")
}

#' @rdname ngrams
#' @importFrom stats complete.cases
#' @export
ngrams.character <- function(x, n = 2L, skip = 0L, concatenator = "_", ...) {
    # trap condition where a "text" is a single NA
    if (is.na(x[1]) && length(x)==1) return(NULL)
    if (any(stringi::stri_detect_fixed(x, " ")) & concatenator != " ")
        stop("whitespace detected: please tokenize() before using ngrams()")
    if (length(x) < min(n)) return(NULL)
    if (identical(as.integer(n), 1L)) {
        if (!identical(as.integer(skip), 0L))
            warning("skip argument ignored for n = 1")
        return(x)
    }
    skipgramcpp(x, n, skip + 1, concatenator)
}


#' @rdname ngrams
#' @export
ngrams.tokenizedTexts <- function(x, n = 2L, skip = 0L, concatenator = "_", ...) {
    ngramsResult <- lapply(x, ngrams.character, n, skip, concatenator)
    # removed mclapply because not faster
    # ngramsResult <- parallel::mclapply(x, ngrams.character, n, skip, concatenator, ...)
    class(ngramsResult) <- c("tokenizedTexts", class(ngramsResult))
    attributes(ngramsResult) <- attributes(x)
    ngramsResult
}

#' @rdname ngrams
#' @examples 
#' tokens <- tokenize(c('a b c d e', 'c d e f g'))
#' tokens_hashed <- hashTokens(tokens)
#' ngrams <- ngrams(tokens, n = 2, skip = 0:1, concatenator = "-")
#' ngrams_hashed <- ngrams(tokens_hashed, n = 2, skip = 0:1, concatenator = "-")
#' as.tokenizedTexts(ngrams_hashed)
#' 
#' '\dontrun{
#' 
#' 
#' tokens2 <- tokenize(head(inaugTexts, 10), removePunct=TRUE)
#' tokens2_hashed <- hashTokens(tokens2)
#' 
#' microbenchmark::microbenchmark(
#'  old=ngrams(tokens2, n = 2:3, skip = 1:2, concatenator = "-"),
#'  new=ngrams(tokens2_hashed, n = 2:3, skip = 1:2, concatenator = "-"),
#'  times=10, unit='relative'
#' )
#' 
#' 
#' Rcpp::sourceCpp('src/ngrams_hashed.cpp')
#' Rcpp::sourceCpp('src/ngrams.cpp')
#' 
#' microbenchmark::microbenchmark(
#'    old=skipgramcpp(tokens2[[1]], 2:3, 1:2, '-'),
#'    new=qatd_cpp_ngram_hashed_vector(tokens2_hashed[[1]], 2:3, 1:2),
#'    times=1, unit='relative'
#' )
#' 
#' 
#' tokens3 <- rep(letters, 50)
#' types3 <- unique(tokens3)
#' tokens3_hashed <- match(tokens3, types3)
#' microbenchmark::microbenchmark(
#'    old=skipgramcpp(tokens3, 2:3, 1:2, '-'),
#'    new=qatd_cpp_ngram_hashed_vector(tokens3_hashed, 2:3, 1:2),
#'    times=10, unit='relative'
#'  )
#' 
#' # Test with greater lexical diversity
#' tokens4 <- paste0(sample(letters, length(tokens3), replace=TRUE), 
#'                   sample(letters, length(tokens3), replace=TRUE))
#' types4 <- unique(tokens4)
#' tokens4_hashed <- match(tokens4, types4)
#' microbenchmark::microbenchmark(
#'    low=qatd_cpp_ngram_hashed_vector(tokens3_hashed, 2:3, 1:2),
#'    high=qatd_cpp_ngram_hashed_vector(tokens4_hashed, 2:3, 1:2),
#'    times=100, unit='relative'
#' )
#' 
#' 
#' # Comparison with tokenizers's skip-grams
#' tokenizers::tokenize_skip_ngrams('a b c d e', n=3, k=1) 
#' # "a c e" "a b c" "b c d" "c d e"
#' ngrams(tokenize('a b c d e'), n=3, skip=0:1, concatenator=' ') 
#' # "a b c" "a b d" "a c d" "a c e" "b c d" "b c e" "b d e" "c d e"
#' 
#'}
#' 
#' @export
ngrams.tokenizedTextsHashed <- function(x, n = 2L, skip = 0L, concatenator = "_", ...) {
  
  # Generate ngrams
  res <- qatd_cpp_ngram_hashed_list(x, n, skip)
  
  # Make character tokens of ngrams
  ngram_ids <- res$id_ngram
  ngram_types <- unlist(lapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , 
                               attr(x, 'vocabulary'), concatenator), use.names=FALSE)
  ngramsResult <- res$text
  attr(ngramsResult, 'vocabulary') <- ngram_types
  class(ngramsResult) <- c("tokenizedTextsHashed")
  return(ngramsResult)
}

unlist(lapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z), attr(res, 'vocabulary'), " "))

#' @rdname ngrams
#' @details Normally, \code{\link{ngrams}} will be called through 
#'   \code{\link{tokenize}}, but these functions are also exported in case a 
#'   user wants to perform lower-level ngram construction on tokenized texts.
#'   
#'   \code{\link{skipgrams}} is a wrapper to \code{\link{ngrams}} that requires 
#'   arguments to be supplied for both \code{n} and \code{skip}.  For \eqn{k}-skip 
#'   skipgrams, set \code{skip} to \code{0:}\eqn{k}, in order to conform to the
#'   definition of skip-grams found in Guthrie et al (2006): A \eqn{k} skip-gram is
#'   an ngram which is a superset of all ngrams and each \eqn{(k-i)} skipgram until
#'   \eqn{(k-i)==0} (which includes 0 skip-grams).
#' @export
#' @references 
#' \href{http://homepages.inf.ed.ac.uk/ballison/pdf/lrec_skipgrams.pdf}{Guthrie,
#' D., B. Allison, W. Liu, and L. Guthrie. 2006. "A Closer Look at Skip-Gram 
#' Modelling."}
#' @importFrom utils combn
#' @examples 
#' tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), 
#'                    removePunct = TRUE, simplify = TRUE)
#' skipgrams(tokens, n = 2, skip = 0:1, concatenator = " ") 
#' skipgrams(tokens, n = 2, skip = 0:2, concatenator = " ") 
#' skipgrams(tokens, n = 3, skip = 0:2, concatenator = " ")   
skipgrams <- function(x, ...) UseMethod("skipgrams")

#' @rdname ngrams
#' @export
skipgrams.character <- function(x, n, skip, concatenator="_", ...)
    ngrams.character(x, n, skip, concatenator)

#' @rdname ngrams
#' @export
skipgrams.tokenizedTexts <- function(x, n, skip, concatenator="_", ...)
    ngrams.tokenizedTexts(x, n, skip, concatenator, ...)

