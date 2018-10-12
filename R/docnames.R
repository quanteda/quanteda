#' Get or set document names
#' 
#' Get or set the document names of a \link{corpus}, \link{tokens}, or \link{dfm} object.
#' @param x the object with docnames
#' @export
#' @return \code{docnames} returns a character vector of the document names
#' @seealso \code{\link{featnames}}
#' @examples
#' # get and set document names to a corpus
#' mycorp <- data_corpus_inaugural
#' docnames(mycorp) <- char_tolower(docnames(mycorp))
#' 
#' # get and set doument names to a tokens
#' mytoks <- tokens(data_corpus_inaugural)
#' docnames(mytoks) <- char_tolower(docnames(mytoks))
#' 
#' # get and set doument names to a dfm
#' mydfm <- dfm(data_corpus_inaugural[1:5])
#' docnames(mydfm) <- char_tolower(docnames(mydfm))
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
    attr(x, "docvars")[["_docid"]]
}

#' @param value a character vector of the same length as \code{x}
#' @return \code{docnames <-} assigns new values to the document names of an object.
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
    x <- as.corpus(x)
    stop(friendly_class_undefined_message(class(x), "docnames<-"))
}

#' @noRd
#' @export
"docnames<-.corpus" <- function(x, value) {
    x <- as.corpus(x)
    attr(x, "docvars")[["_docid"]] <- value
    return(x)
}

#' @noRd
#' @export
"docnames<-.tokens" <- function(x, value) {
    names(x) <- attr(x, "docvars")[["_docid"]] <- value
    return(x)
}

#' @noRd
#' @export
"docnames<-.dfm" <- function(x, value) {
    rownames(x) <- attr(x, "docvars")[["_docid"]] <- value
    return(x)
}

