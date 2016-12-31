#' deprecated function name for forming ngrams and skipgrams
#' 
#' Deprecated function names for forming ngrams and skipgrams; use 
#' \code{\link{tokens_ngrams}} and \code{\link{tokens_skipgrams}} instead.
#' @keywords internal tokens deprecated
#' @export
ngrams <- function(x, ...) {
    .Deprecated("tokens_ngrams")
    tokens_ngrams(x, ...)
}
# @examples 
# txt <- c("a b c d e", "c d e f g")
# toks <- tokens(txt)
# ngrams(toks, n = 2:3)

#' @rdname ngrams
#' @export
ngrams.default <-  function(x, ...) {
    tokens_ngrams(x, ...)
}

#' @rdname ngrams
#' @export
ngrams.tokenizedTexts <- function(x, ...) {
    as.tokenizedTexts(tokens_ngrams(as.tokens(x), ...))
}

#' @rdname ngrams
#' @keywords internal tokens deprecated
#' @export
skipgrams <- function(x, ...) {
    ngrams(x, ...)
}


#' create ngrams and skipgrams from tokens
#' 
#' Create a set of ngrams (tokens in sequence) from already tokenized text
#' objects, with an optional skip argument to form skipgrams. Both the ngram
#' length and the skip lengths take vectors of arguments to form multiple
#' lengths or skips in one pass.  Implemented in C++ for efficiency.
#' @author Kohei Watanabe (C++) and Ken Benoit (R)
#' @return a tokens object consisting a list of character vectors of ngrams, one
#'   list element per text, or a character vector if called on a simple
#'   character vector
#' @param x a tokens object, or a character vector, or a list of characters
#' @param n integer vector specifying the number of elements to be concatenated 
#'   in each ngram.  Each element of this vector will define a $n$ in the 
#'   $n$-gram(s) that are produced.
#' @param skip integer vector specifying the adjacency skip size for tokens 
#'   forming the ngrams, default is 0 for only immediately neighbouring words. 
#'   For \code{skipgrams}, \code{skip} can be a vector of integers, as the 
#'   "classic" approach to forming skip-grams is to set skip = \eqn{k} where 
#'   \eqn{k} is the distance for which \eqn{k} or fewer skips are used to 
#'   construct the \eqn{n}-gram.  Thus a "4-skip-n-gram" defined as \code{skip =
#'   0:4} produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0
#'   skips (where 0 skips are typical n-grams formed from adjacent words).  See 
#'   Guthrie et al (2006).
#' @param concatenator character for combining words, default is \code{_} 
#'   (underscore) character
#' @details Normally, these functions will be called through 
#'   \code{\link{tokens}(x, ngrams = , ...)}, but these functions are provided 
#'   in case a user wants to perform lower-level ngram construction on tokenized
#'   texts.
#' @export
#' @examples
#' # ngrams
#' tokens_ngrams(tokens(c("a b c d e", "c d e f g")), n = 2:3)
#' 
#' toks <- tokens(c(text1 = "the quick brown fox jumped over the lazy dog"))
#' tokens_ngrams(toks, n = 1:3)
#' tokens_ngrams(toks, n = c(2,4), concatenator = " ")
#' tokens_ngrams(toks, n = c(2,4), skip = 1, concatenator = " ")
#' 
tokens_ngrams <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    UseMethod("tokens_ngrams")
}

#' @rdname ngrams
#' @importFrom stats complete.cases
#' @noRd
#' @export
tokens_ngrams.character <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    tokens_ngrams(tokens(x), n = n, skip = skip, concatenator = concatenator)
}

#' @rdname tokens_ngrams
#' @note \code{char_ngrams} is a convenience wrapper for a (non-list) 
#'   vector of characters, so named to be consistent with \pkg{quanteda}'s naming
#'   scheme.
#' @export
char_ngrams <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    if (!is.character(x))
        stop("x must be a character object")
    tokens_ngrams(x, n, skip, concatenator)
}
    

#' @rdname tokens_ngrams
#' @noRd
#' @examples 
#' txt <- c("a b c d e", "c d e f g")
#' toks <- tokens(txt)
#' tokens_ngrams(toks, n = 2:3)
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_ngrams.tokens <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    
    if (any(n <= 0)) stop("ngram length has to be greater than zero")
    
    #types_org <- types(x)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    # Generate ngrams
    x <- qatd_cpp_tokens_ngrams(x, types(x), concatenator, n, skip + 1)

    types_ngm <- attr(x, 'types')
    if(length(types_ngm) > 0){
        types_ngm <- stringi::stri_encode(types_ngm, "", "UTF-8") # types() cannot be used
    }
    attributes(x) <- attrs_org
    types(x) <- types_ngm
    names(x) <- names_org
    attr(x, "ngrams") <- as.integer(n)
    attr(x, "skip") <- as.integer(skip)
    attr(x, "concatenator") <- concatenator
    return(x)
}


#' @rdname tokens_ngrams
#' @noRd
#' @export
tokens_ngrams.tokenizedTexts <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    as.tokenizedTexts(tokens_ngrams(as.tokens(x), n = n, skip = skip, concatenator = concatenator))
}


#' @rdname tokens_ngrams
#' @details 
#'   \code{\link{tokens_skipgrams}} is a wrapper to \code{\link{ngrams}} that requires 
#'   arguments to be supplied for both \code{n} and \code{skip}.  For
#'   \eqn{k}-skip skipgrams, set \code{skip} to \code{0:}\eqn{k}, in order to
#'   conform to the definition of skip-grams found in Guthrie et al (2006): A
#'   \eqn{k} skip-gram is an ngram which is a superset of all ngrams and each
#'   \eqn{(k-i)} skipgram until \eqn{(k-i)==0} (which includes 0 skip-grams).
#' @export
#' @references 
#' \href{http://homepages.inf.ed.ac.uk/ballison/pdf/lrec_skipgrams.pdf}{Guthrie,
#' D., B. Allison, W. Liu, and L. Guthrie. 2006. "A Closer Look at Skip-Gram 
#' Modelling."}
#' @importFrom utils combn
#' @examples 
#' # skipgrams
#' toks <- tokens("insurgents killed in ongoing fighting")
#' tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = " ") 
#' tokens_skipgrams(toks, n = 2, skip = 0:2, concatenator = " ") 
#' tokens_skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")   
tokens_skipgrams <- function(x, n, skip, concatenator="_") {
    tokens_ngrams(x, n = n, skip = skip, concatenator = concatenator)
}

