# head
# tail

# kwic
# 
# char_tolower
# char_toupper

#' count the number of documents or features
#' 
#' Get the number of documents, tokens, types, or features in an object.
#' @details \code{ndoc} returns the number of documents or features in a 
#'   quanteda object.
#' @param x a \pkg{quanteda} object: a \link{corpus}, \link{dfm}, or \link{tokens}
#'   object, or a readtext object from the \pkg{readtext} package.
#' @return an integer (count) of the number of documents or features in the 
#'   corpus or dfm
#' @export
#' @examples 
#' # number of documents
#' ndoc(data_corpus_inaugural)
#' ndoc(corpus_subset(data_corpus_inaugural, Year > 1980))
#' ndoc(tokens(data_corpus_inaugural))
#' ndoc(dfm(corpus_subset(data_corpus_inaugural, Year > 1980)))
#' 
ndoc <- function(x) {
    UseMethod("ndoc")
}

#' @noRd
#' @export
ndoc.corpus <- function(x) {
    nrow(documents(x))
}

#' @noRd
#' @export
ndoc.dfm <- function(x) {
    nrow(x)
}

#' @export
#' @noRd
ndoc.tokens <- function(x) {
    length(x)
}

#' @export
#' @noRd
ndoc.tokenizedTexts <- function(x) {
    length(x)
}


#' @rdname ndoc
#' @details \code{nfeature} returns the number of features from a dfm; it is an
#'   alias for \code{ntype} when applied to dfm objects.  This function is only 
#'   defined for \link{dfm} objects because only these have "features".  (To count
#'   tokens, see \code{\link{ntoken}})
#' @export
#' @seealso \code{\link{ntoken}}
#' @examples
#' # number of features
#' nfeature(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), removePunct = FALSE))
#' nfeature(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), removePunct = TRUE))
nfeature <- function(x) {
    UseMethod("nfeature")
}

#' @noRd
#' @export
nfeature.dfm <- function(x) {
    ncol(x)
}


#' count the number of tokens or types
#' 
#' Get the count of tokens (total features) or types (unique tokens)
#' in a text, corpus, or dfm.
#' @param x a \pkg{quanteda} object: a character, \link{corpus}, or
#'   \link{tokens} object
#' @param ... additional arguments passed to \code{\link{tokens}}
#' @note Due to differences between raw text tokens and features that have been 
#'   defined for a \link{dfm}, the counts be different for dfm objects and the 
#'   texts from which the dfm was generated.  Because the method tokenizes the 
#'   text in order to count the tokens, your results will depend on the options 
#'   passed through to \code{\link{tokenize}}
#' @return count of the total tokens or types
#' @details
#' The precise definition of "tokens" for objects not yet tokenized (e.g.
#' \link{character} or \link{corpus} objects can be controlled through optional
#' arguments passed to \code{\link{tokens}} through \code{...}.
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
#' ntoken(txt)
#' ntype(txt)
#' ntoken(toLower(txt))  # same
#' ntype(toLower(txt))   # fewer types
#' ntoken(toLower(txt), removePunct = TRUE)
#' ntype(toLower(txt), removePunct = TRUE)
#' 
#' # with some real texts
#' ntoken(corpus_subset(data_corpus_inaugural, Year<1806), removePunct = TRUE)
#' ntype(corpus_subset(data_corpus_inaugural, Year<1806), removePunct = TRUE)
#' ntoken(dfm(corpus_subset(data_corpus_inaugural, Year<1800)))
#' ntype(dfm(corpus_subset(data_corpus_inaugural, Year<1800)))
#' @export
ntoken <- function(x, ...) {
    UseMethod("ntoken")
}

#' @rdname ntoken
#' @details 
#' For \link{dfm} objects, \code{ntype} will only return the count of features
#' that occur more than zero times in the dfm.
#' @export
ntype <- function(x, ...) {
    UseMethod("ntype")
}

#' @noRd
#' @export
ntoken.corpus <- function(x, ...) {
    ntoken(texts(x), ...)
}


#' @noRd
#' @export
ntoken.character <- function(x, ...) {
    ntoken(tokenize(x, ...))
}

#' @noRd
#' @export
ntoken.dfm <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("additional arguments not used for ntoken.dfm()")
    rowSums(x)
}

#' @noRd
#' @export
ntoken.tokenizedTexts <- function(x, ...) {
    lengths(x)
}

#' @noRd
#' @export
ntype.character <- function(x, ...) {
    ntype(tokenize(x, ...))
}

#' @noRd
#' @export
ntype.corpus <- function(x, ...) {
    ntype(texts(x), ...)
}


#' @noRd
#' @export
ntype.dfm <- function(x, ...) {
    ## only returns total NON-ZERO COUNT types
    sum(colSums(x) > 0)
}

#' @noRd
#' @export
ntype.tokenizedTexts <- function(x, ...) {
    sapply(lapply(x, unique), length)
}

#' @export
#' @noRd
ntoken.tokens <- function(x, ...) {
    #lengths(x)
    lengths(unclass(x)) # this is 1000 times faster
}

#' @export
#' @noRd
ntype.tokens <- function(x, ...) {
    length(types(x))
}


#' count the number of sentences
#' 
#' Return the count of sentences in a corpus or character object.
#' @param x texts or corpus whose sentences will be counted
#' @param ... additional arguments passed to \code{\link{tokens}}
#' @note `nsentence()` relies on the boundaries definitions in the \pkg{stringi}
#'   package (see \link[stringi]{stri_opts_brkiter}).  It does not count
#'   sentences correctly if the text has been transformed to lower case, and for
#'   this reason \code{nsentence} will issue a warning if it detects all
#'   lower-cased text.
#' @return count(s) of the total sentences per text
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence: second part of first sentence.",
#'          text2 = "A word. Repeated repeated.",
#'          text3 = "Mr. Jones has a PhD from the LSE.  Second sentence.")
#' nsentence(txt)
#' @export
nsentence <- function(x, ...) {
    UseMethod("nsentence")
}

#' @noRd
#' @export
nsentence.character <- function(x, ...) {
    upcase <- try(any(stringi::stri_detect_charclass(x, "[A-Z]")), silent = TRUE)
    if (!is.logical(upcase)) {
        # warning("Input text contains non-UTF-8 characters.")
    }
    else if (!upcase)
        warning("nsentence() does not correctly count sentences in all lower-cased text")
    lengths(tokenize(x, what = "sentence", ...))
}

#' @noRd
#' @export
nsentence.corpus <- function(x, ...) {
    nsentence(texts(x), ...)
}
