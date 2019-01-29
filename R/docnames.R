#' Get or set document names
#' 
#' Get or set the document names of a \link{corpus}, \link{tokens}, or \link{dfm} object.
#' @param x the object with docnames
#' @export
#' @return \code{docnames} returns a character vector of the document names
#' @seealso \code{\link{featnames}}
#' @examples
#' # get and set doument names to a corpus
#' corp <- data_corpus_inaugural
#' docnames(corp) <- char_tolower(docnames(corp))
#' 
#' # get and set doument names to a tokens
#' toks <- tokens(data_corpus_inaugural)
#' docnames(toks) <- char_tolower(docnames(toks))
#' 
#' # get and set doument names to a dfm
#' dfmat <- dfm(data_corpus_inaugural[1:5])
#' docnames(dfmat) <- char_tolower(docnames(dfmat))
#' 
#' @keywords corpus dfm
docnames <- function(x) {
    UseMethod("docnames")
}

#' @export
docnames.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "docnames"))
}

#' @noRd
#' @export
docnames.corpus <- function(x) {
    x <- as.corpus(x)
    get_docvars(x, "docname_", FALSE, TRUE, TRUE)
}

#' @param value a character vector of the same length as \code{x}
#' @return \code{docnames <-} assigns new values to the document names of an object.  
#' docnames can only be character, so any non-character value assigned to be a
#' docname will be coerced to mode `character`.
#' @export
#' @examples 
#' # reassign the document names of the inaugural speech corpus
#' docnames(data_corpus_inaugural) <- paste("Speech", 1:ndoc(data_corpus_inaugural), sep="")
#' 
#' @rdname docnames
"docnames<-" <- function(x, value) {
    UseMethod("docnames<-")
}

#' @export
"docnames<-.default" <- function(x, value) {
    stop(friendly_class_undefined_message(class(x), "docnames<-"))
}

#' @noRd
#' @export
"docnames<-.corpus" <- function(x, value) {
    x <- as.corpus(x)
    names(x) <- attr(x, "docvars")[["docname_"]] <- value
    return(x)
}

#' @noRd
#' @export
"docnames<-.tokens" <- function(x, value) {
    x <- as.tokens(x)
    names(x) <- attr(x, "docvars")[["docname_"]] <- value
    return(x)
}

#' @noRd
#' @export
"docnames<-.dfm" <- function(x, value) {
    x <- as.dfm(x)
    rownames(x) <- attr(x, "docvars")[["docname_"]] <- value
    return(x)
}
