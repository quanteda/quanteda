#' Create n-grams and skip-grams from tokens
#'
#' Create a set of n-grams (tokens in sequence) from already tokenized text
#' objects, with an optional skip argument to form skip-grams. Both the n-gram
#' length and the skip lengths take vectors of arguments to form multiple
#' lengths or skips in one pass.  Implemented in C++ for efficiency.
#' @return a tokens object consisting a list of character vectors of n-grams, one
#'   list element per text, or a character vector if called on a simple
#'   character vector
#' @param x a tokens object, or a character vector, or a list of characters
#' @param n integer vector specifying the number of elements to be concatenated
#'   in each n-gram.  Each element of this vector will define a \eqn{n} in the
#'   \eqn{n}-gram(s) that are produced.
#' @param skip integer vector specifying the adjacency skip size for tokens
#'   forming the n-grams, default is 0 for only immediately neighbouring words.
#'   For `skipgrams`, `skip` can be a vector of integers, as the
#'   "classic" approach to forming skip-grams is to set skip = \eqn{k} where
#'   \eqn{k} is the distance for which \eqn{k} or fewer skips are used to
#'   construct the \eqn{n}-gram.  Thus a "4-skip-n-gram" defined as `skip =
#'   0:4` produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0
#'   skips (where 0 skips are typical n-grams formed from adjacent words).  See
#'   Guthrie et al (2006).
#' @param concatenator character for combining words, default is `_`
#'   (underscore) character
#' @inheritParams messages
#' @details Normally, these functions will be called through
#'   `[tokens](x, ngrams = , ...)`, but these functions are provided
#'   in case a user wants to perform lower-level n-gram construction on tokenized
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
tokens_ngrams <- function(x, n = 2L, skip = 0L, concatenator = concat(x),
                          verbose = quanteda_options("verbose")) {
    UseMethod("tokens_ngrams")
}

#' @export
tokens_ngrams.default <- function(x, n = 2L, skip = 0L, concatenator = concat(x),
                                  verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_ngrams")
}

#' @rdname tokens_ngrams
#' @note `char_ngrams` is a convenience wrapper for a (non-list)
#'   vector of characters, so named to be consistent with \pkg{quanteda}'s naming
#'   scheme.
#' @export
char_ngrams <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    UseMethod("char_ngrams")
}

#' @export
char_ngrams.default <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    check_class(class(x), "char_ngrams")
}

#' @export
char_ngrams.character <- function(x, n = 2L, skip = 0L, concatenator = "_") {
    lifecycle::deprecate_soft("4.0.0", 
                              I('char_ngrams()'), 
                              I('`tokens_ngram(tokens(x))`'))
    if (any(stringi::stri_detect_charclass(x, "\\p{Z}")) & concatenator != " ")
        warning("whitespace detected: you may need to run tokens() first")
    x <- as.tokens(list(x))
    as.list(tokens_ngrams(x, n = n, skip = skip, concatenator = concatenator))[[1]]
}


#' @rdname tokens_ngrams
#' @noRd
#' @examples
#' txt <- c(txt1 = "a b c d e", txt2 = "c d e f g")
#' toks <- tokens(txt)
#' tokens_ngrams(toks, n = 2:3)
#' @export
tokens_ngrams.tokens_xptr <- function(x, n = 2L, skip = 0L, concatenator = concat(x),
                                      verbose = quanteda_options("verbose")) {

    n <- check_integer(n, min = 1, max_len = Inf)
    skip <- check_integer(skip, min_len = 1, max_len = Inf, min = 0)
    concatenator <- check_character(concatenator)
    verbose <- check_logical(verbose)

    attrs <- attributes(x)
    if (identical(n, 1L) && identical(skip, 0L))
        return(x)
    if (verbose)
        before <- stats_tokens(x)
    result <- cpp_tokens_ngrams(x, concatenator, n, skip, get_threads())
    field_object(attrs, "ngram") <- n
    field_object(attrs, "skip") <- skip
    field_object(attrs, "concatenator") <- concatenator
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_ngrams()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_ngrams.tokens <- function(x, ...) {
    as.tokens(tokens_ngrams(as.tokens_xptr(x), ...))
}

#' @rdname tokens_ngrams
#' @details
#'   [tokens_skipgrams()] is a wrapper to [tokens_ngrams()] that requires
#'   arguments to be supplied for both `n` and `skip`. For \eqn{k}-skip
#'   skip-grams, set `skip` to `0:`\eqn{k}, in order to conform to the
#'   definition of skip-grams found in Guthrie et al (2006): A \eqn{k} skip-gram
#'   is an n-gram which is a superset of all n-grams and each \eqn{(k-i)}
#'   skip-gram until \eqn{(k-i)==0} (which includes 0 skip-grams).
#' @export
#' @references
#' Guthrie, David, Ben Allison, Wei Liu, Louise Guthrie, and Yorick Wilks. 2006.
#' "A Closer Look at Skip-Gram Modelling." `https://aclanthology.org/L06-1210/`
#' @importFrom utils combn
#' @examples
#' # skipgrams
#' toks <- tokens("insurgents killed in ongoing fighting")
#' tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = " ")
#' tokens_skipgrams(toks, n = 2, skip = 0:2, concatenator = " ")
#' tokens_skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")
tokens_skipgrams <- function(x, n, skip, concatenator = concat(x),
                             verbose = quanteda_options("verbose")) {
    UseMethod("tokens_skipgrams")
}

#' @export
tokens_skipgrams.default <- function(x, n, skip, concatenator = concat(x),
                                     verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_skipgrams")
}

#' @export
tokens_skipgrams.tokens <- tokens_ngrams.tokens

#' @export
tokens_skipgrams.tokens_xptr <- tokens_ngrams.tokens_xptr

