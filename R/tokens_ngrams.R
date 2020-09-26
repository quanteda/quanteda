#' Create ngrams and skipgrams from tokens
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
#'   in each ngram.  Each element of this vector will define a \eqn{n} in the
#'   \eqn{n}-gram(s) that are produced.
#' @param skip integer vector specifying the adjacency skip size for tokens
#'   forming the ngrams, default is 0 for only immediately neighbouring words.
#'   For `skipgrams`, `skip` can be a vector of integers, as the
#'   "classic" approach to forming skip-grams is to set skip = \eqn{k} where
#'   \eqn{k} is the distance for which \eqn{k} or fewer skips are used to
#'   construct the \eqn{n}-gram.  Thus a "4-skip-n-gram" defined as `skip =
#'   0:4` produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0
#'   skips (where 0 skips are typical n-grams formed from adjacent words).  See
#'   Guthrie et al (2006).
#' @param concatenator character for combining words, default is `_`
#'   (underscore) character
#' @details Normally, these functions will be called through
#'   `[tokens](x, ngrams = , ...)`, but these functions are provided
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
tokens_ngrams <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    UseMethod("tokens_ngrams")
}

#' @export
tokens_ngrams.default <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    stop(friendly_class_undefined_message(class(x), "tokens_ngrams"))
}

## this function is not exported because it should not exist - it violates
## the grammatrical rules of quanteda (inputs character, outputs tokens),
## but starts with "tokens_"
#' @importFrom stats complete.cases
tokens_ngrams.character <- function(x, n = 2L, skip = 0L, concatenator = "_") {

    # trap condition where a "text" is a single NA
    if (is.na(x[1]) && length(x) == 1) return(NULL)
    if (any(stringi::stri_detect_charclass(x, "\\p{Z}")) & concatenator != " ")
        warning("whitespace detected: you may need to run tokens() first")
    if (length(x) < min(n)) return(NULL)
    if (identical(as.integer(n), 1L)) {
        if (!identical(as.integer(skip), 0L))
            warning("skip argument ignored for n = 1")
        return(x)
    }
    # converts the character to a tokens object, and returns just the first "document"
    # as a character vector
    tokens_ngrams(as.tokens(list(x)), n = n, skip = skip, concatenator = concatenator)[[1]]
}

#' @rdname tokens_ngrams
#' @note `char_ngrams` is a convenience wrapper for a (non-list)
#'   vector of characters, so named to be consistent with \pkg{quanteda}'s naming
#'   scheme.
#' @examples
#' # on character
#' char_ngrams(letters[1:3], n = 1:3)
#'
#' @export
char_ngrams <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    UseMethod("char_ngrams")
}

#' @export
char_ngrams.default <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    stop(friendly_class_undefined_message(class(x), "char_ngrams"))
}

#' @export
char_ngrams.character <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    as.character(tokens_ngrams(x, n, skip, concatenator))
}


#' @rdname tokens_ngrams
#' @noRd
#' @examples
#' txt <- c(txt1 = "a b c d e", txt2 = "c d e f g")
#' toks <- tokens(txt)
#' tokens_ngrams(toks, n = 2:3)
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_ngrams.tokens <- function(x, n = 2L, skip = 0L, concatenator = "_") {

    x <- as.tokens(x)
    n <- as.integer(n)
    skip <- as.integer(skip)

    if (any(n <= 0L))
        stop("ngram length has to be greater than zero")

    attrs <- attributes(x)
    if (identical(n, 1L) && identical(skip, 0L))
        return(x)
    result <- qatd_cpp_tokens_ngrams(x, types(x), concatenator, n, skip + 1L)
    field_object(attrs, "ngram") <- n
    field_object(attrs, "skip") <- skip
    field_object(attrs, "concatenator") <- concatenator
    rebuild_tokens(result, attrs)
}

#' @rdname tokens_ngrams
#' @details
#'   [tokens_skipgrams()] is a wrapper to [tokens_ngrams()]
#'   that requires arguments to be supplied for both `n` and `skip`.
#'   For \eqn{k}-skip skipgrams, set `skip` to `0:`\eqn{k}, in order
#'   to conform to the definition of skip-grams found in Guthrie et al (2006): A
#'   \eqn{k} skip-gram is an ngram which is a superset of all ngrams and each
#'   \eqn{(k-i)} skipgram until \eqn{(k-i)==0} (which includes 0 skip-grams).
#' @export
#' @references
#' Guthrie, David, Ben Allison, Wei Liu, Louise Guthrie, and Yorick Wilks. 2006.
#' "[A Closer
#' Look at Skip-Gram Modelling](https://www.aclweb.org/anthology/L06-1210/)."
#' @importFrom utils combn
#' @examples
#' # skipgrams
#' toks <- tokens("insurgents killed in ongoing fighting")
#' tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = " ")
#' tokens_skipgrams(toks, n = 2, skip = 0:2, concatenator = " ")
#' tokens_skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")
tokens_skipgrams <- function(x, n, skip, concatenator = "_") {
    UseMethod("tokens_skipgrams")
}

#' @export
tokens_skipgrams.default <- function(x, n, skip, concatenator = "_") {
    stop(friendly_class_undefined_message(class(x), "tokens_skipgrams"))
}

#' @export
tokens_skipgrams.tokens <- function(x, n, skip, concatenator = "_") {
    tokens_ngrams(x, n = n, skip = skip, concatenator = concatenator)
}
