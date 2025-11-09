#' Count the number of documents or features
#'
#' Get the number of documents or features in an object.
#' @param x a \pkg{quanteda} object: a [corpus], [dfm], 
#'   [tokens], or [tokens_xptr] object
#' @param x a \pkg{quanteda} object: a [corpus], [dfm], [tokens], or
#'   [tokens_xptr] object, or a readtext object from the \pkg{readtext} package
#' @returns `ndoc()` returns an integer count of the number of documents in an
#'   object whose texts are organized as "documents" (a [corpus], [dfm], or
#'   [tokens]/[tokens_xptr] object.
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
#' @returns `nfeat()` returns an integer count of the number of features.  It is
#'   an alias for `ntype()` for a dfm. This function is only defined for [dfm]
#'   objects because only these have "features".
#' @export
#' @seealso [ntoken()], [ntype()]
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
#' @param x a \pkg{quanteda} [tokens] or [dfm] object
#' @param ... additional arguments passed to `tokens()`
#' @returns `ntoken()` returns a named integer vector of the counts of the total
#'   tokens
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
#' toks <- tokens(txt)
#' ntoken(toks)
#' ntype(toks)
#' ntoken(tokens_tolower(toks))  # same
#' ntype(tokens_tolower(toks))   # fewer types
#'
#' # with some real texts
#' toks <- tokens(corpus_subset(data_corpus_inaugural, Year < 1806))
#' ntoken(tokens(toks, remove_punct = TRUE))
#' ntype(tokens(toks, remove_punct = TRUE))
#' ntoken(dfm(toks))
#' ntype(dfm(toks))
#' @export
ntoken <- function(x, ...) {
    UseMethod("ntoken")
}

#' @export
ntoken.default <- function(x, ...) {
    check_class(class(x), "ntoken")
}

#' @export
ntoken.corpus <- function(x, ...) {
    x <- as.corpus(x)
    ntoken(as.character(x), ...)
}

#' @export
ntoken.character <- function(x, ...) {
    lifecycle::deprecate_soft("4.0.0", I('ntoken.character()/ntype.corpus()'), 
                              I('ntoken(tokens(x))'))
    ntoken(tokens(x, ...))
}

#' @export
ntoken.tokens <- function(x, remove_padding = FALSE, ...) {
    ntoken(as.tokens_xptr(x), remove_padding, ...)
}

#' @export
ntoken.dfm <- function(x, ...) {
    
    x <- as.dfm(x)
    check_dots(...)
    
    result <- as.integer(rowSums(x))
    names(result) <- docnames(x)
    result
}

#' @rdname ntoken
#' @returns
#' `ntypes()` returns a named integer vector of the counts of the types (unique
#' tokens) per document.  For [dfm] objects, `ntype()` will only return the
#' count of features that occur more than zero times in the dfm.
#' @export
ntype <- function(x, ...) {
    UseMethod("ntype")
}

#' @export
ntype.default <- function(x, ...) {
    check_class(class(x), "ntype")
}

#' @export
ntype.character <- function(x, ...) {
    lifecycle::deprecate_soft("4.0.0", 
                              I('ntype.character()/ntype.corpus()'), 
                              I('ntoken(tokens(x))'))
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
ntype.dfm <- function(x, remove_padding = FALSE, ...) {
    x <- as.dfm(x)
    remove_padding <- check_logical(remove_padding)
    check_dots(...)
    
    if (remove_padding)
        x <- dfm_remove(x, "")
    
    # only returns total non-zero features
    result <- as.integer(rowSums(x > 0))
    names(result) <- docnames(x)
    return(result)
}

#' @export
ntype.tokens <- function(x, remove_padding = FALSE, ...) {
    ntype(as.tokens_xptr(x), remove_padding, ...)
}


#' Count the number of sentences
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#' 
#' Return the count of sentences in a corpus or character object.
#' @param x a character or [corpus] whose sentences will be counted
#' @note 
#'   `nsentence()` is now deprecated for all usages except tokens objects that
#'   have already been tokenised with `tokens(x, what = "sentence")`.  Using it
#'   on character or corpus objects will now generate a warning.
#' 
#'   `nsentence()` relies on the boundaries definitions in the \pkg{stringi}
#'   package (see [stri_opts_brkiter][stringi::stri_opts_brkiter]).  It does not
#'   count sentences correctly if the text has been transformed to lower case,
#'   and for this reason `nsentence()` will issue a warning if it detects all
#'   lower-cased text.
#' @return count(s) of the total sentences per text
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence: second part of first sentence.",
#'          text2 = "A word. Repeated repeated.",
#'          text3 = "Mr. Jones has a PhD from the LSE.  Second sentence.")
#' tokens(txt, what = "sentence") |>
#'     nsentence()
#' @export
nsentence <- function(x) {
    UseMethod("nsentence")
}

#' @export
nsentence.default <- function(x) {
    check_class(class(x), "nsentence")
}

#' @export
#' @importFrom stringi stri_detect_charclass
nsentence.character <- function(x) {
    lifecycle::deprecate_soft("4.0", 
                              "nsentence()", 
                              I('lengths(tokens(what = "sentence"))'))
    upcase <- try(any(stri_detect_charclass(x, "[A-Z]")), silent = TRUE)
    if (!is.logical(upcase)) {
        # warning("Input text contains non-UTF-8 characters.")
    } else if (!upcase)
        warning("nsentence() does not correctly count sentences in all lower-cased text")
    ntoken(tokens(x, what = "sentence"))
}

#' @export
nsentence.corpus <- function(x) {
    x <- as.corpus(x)
    nsentence(as.character(x))
}

#' @export
nsentence.tokens <- function(x) {
    x <- as.tokens(x)
    attrs <- attributes(x)
    if (field_object(attrs, "what") != "sentence")
        stop("nsentence on a tokens object only works if what = \"sentence\"")
    return(lengths(x))
}
