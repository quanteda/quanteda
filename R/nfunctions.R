#' Count the number of documents or features
#'
#' Get the number of documents or features in an object.
#' @details `ndoc` returns the number of documents in an object
#'   whose texts are organized as "documents" (a [corpus],
#'   [dfm], or [tokens] object, a readtext object from the
#'   \pkg{readtext} package).
#'
#' @param x a \pkg{quanteda} object: a [corpus], [dfm], or
#'   [tokens] object, or a readtext object from the \pkg{readtext} package.
#' @return an integer (count) of the number of documents or features
#' @export
#' @examples
#' # number of documents
#' ndoc(data_corpus_inaugural)
#' ndoc(corpus_subset(data_corpus_inaugural, Year > 1980))
#' ndoc(tokens(data_corpus_inaugural))
#' ndoc(dfm(tokens(corpus_subset(data_corpus_inaugural, Year > 1980))))
#'
ndoc <- function(x) {
    UseMethod("ndoc")
}

#' @export
ndoc.default <- function(x) {
    check_class(class(x), "ndoc")
}

#' @export
ndoc.corpus <- function(x) {
    length(as.corpus(x))
}

#' @export
ndoc.dfm <- function(x) {
    nrow(as.dfm(x))
}

#' @export
ndoc.tokens <- function(x) {
    length(as.tokens(x))
}


#' @rdname ndoc
#' @details `nfeat` returns the number of features from a dfm; it is an
#'   alias for `ntype` when applied to dfm objects.  This function is only
#'   defined for [dfm] objects because only these have "features".  (To count
#'   tokens, see [ntoken()].)
#' @export
#' @seealso [ntoken()]
#' @examples
#' # number of features
#' toks1 <- tokens(corpus_subset(data_corpus_inaugural, Year > 1980), remove_punct = FALSE)
#' toks2 <- tokens(corpus_subset(data_corpus_inaugural, Year > 1980), remove_punct = TRUE)
#' nfeat(dfm(toks1))
#' nfeat(dfm(toks2))
nfeat <- function(x) {
    UseMethod("nfeat")
}

#' @export
nfeat.default <- function(x) {
    check_class(class(x), "nfeat")
}

#' @export
nfeat.dfm <- function(x) {
    x <- as.dfm(x)
    ncol(x)
}

#' Count the number of tokens or types
#'
#' Get the count of tokens (total features) or types (unique tokens).
#' @param x a \pkg{quanteda} object: a character, [corpus],
#'   [tokens], or [dfm] object
#' @param ... additional arguments passed to [tokens()]
#' @note Due to differences between raw text tokens and features that have been
#'   defined for a [dfm], the counts may be different for dfm objects and the
#'   texts from which the dfm was generated.  Because the method tokenizes the
#'   text in order to count the tokens, your results will depend on the options
#'   passed through to [tokens()].
#' @return named integer vector of the counts of the total tokens or types
#' @details
#' The precise definition of "tokens" for objects not yet tokenized (e.g.
#' [character] or [corpus] objects) can be controlled through optional
#' arguments passed to [tokens()] through `...`.
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
#' ntoken(txt)
#' ntype(txt)
#' ntoken(char_tolower(txt))  # same
#' ntype(char_tolower(txt))   # fewer types
#' ntoken(char_tolower(txt), remove_punct = TRUE)
#' ntype(char_tolower(txt), remove_punct = TRUE)
#'
#' # with some real texts
#' ntoken(corpus_subset(data_corpus_inaugural, Year < 1806), remove_punct = TRUE)
#' ntype(corpus_subset(data_corpus_inaugural, Year < 1806), remove_punct = TRUE)
#' ntoken(dfm(tokens(corpus_subset(data_corpus_inaugural, Year < 1800))))
#' ntype(dfm(tokens(corpus_subset(data_corpus_inaugural, Year < 1800))))
#' @export
ntoken <- function(x, ...) {
    UseMethod("ntoken")
}

#' @export
ntoken.default <- function(x, ...) {
    check_class(class(x), "ntoken")
}

#' @rdname ntoken
#' @details
#' For [dfm] objects, `ntype` will only return the count of features
#' that occur more than zero times in the dfm.
#' @export
ntype <- function(x, ...) {
    UseMethod("ntype")
}

#' @export
ntype.default <- function(x, ...) {
    check_class(class(x), "ntype")
}

#' @export
ntoken.corpus <- function(x, ...) {
    x <- as.corpus(x)
    ntoken(as.character(x), ...)
}

#' @export
ntoken.character <- function(x, ...) {
    ntoken(tokens(x, ...))
}

#' @export
ntoken.tokens <- function(x, ...) {
    x <- as.tokens(x)
    if (length(list(...))) {
        lengths(tokens(x, ...))
    } else {
        lengths(x)
    }
}

#' @export
ntoken.dfm <- function(x, ...) {
    
    x <- as.dfm(x)
    check_dots(...)
    
    result <- as.integer(rowSums(x))
    names(result) <- docnames(x)
    result
}

#' @export
ntype.character <- function(x, ...) {
    if (length(list(...)))
        ntype(tokens(x, ...))
    else
        ntype(tokens(x))
}

#' @export
ntype.corpus <- function(x, ...) {
    x <- as.corpus(x)
    ntype(as.character(x), ...)
}

#' @export
ntype.dfm <- function(x, ...) {
    
    x <- as.dfm(x)
    check_dots(...)
    
    # only returns total non-zero features
    result <- as.integer(rowSums(x > 0))
    names(result) <- docnames(x)
    result
}

#' @export
ntype.tokens <- function(x, ...) {
    if (length(list(...)))
        x <- tokens(x, ...)
    vapply(unclass(x), function(y) length(unique(y[y > 0])), integer(1))
}
