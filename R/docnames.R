#' Get or set document names
#' 
#' Get or set the document names of a [corpus], [tokens], or [dfm] object.
#' @param x the object with docnames
#' @export
#' @return `docnames` returns a character vector of the document names
#' @seealso [featnames()]
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

#' @noRd
#' @export
docnames.tokens <- function(x) {
    get_docvars(x, "docname_", FALSE, TRUE, TRUE)
}

#' @param value a character vector of the same length as `x`
#' @return `docnames <-` assigns new values to the document names of an object.  
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
    temp <- make_docvars(length(value), value, TRUE)
    attr(x, "docvars")[c("docname_", "docid_", "segid_")] <- temp
    attr(x, "names") <- temp[["docname_"]]
    return(x)
}

#' @noRd
#' @export
"docnames<-.tokens" <- function(x, value) {
    x <- as.tokens(x)
    temp <- make_docvars(length(value), value, TRUE)
    attr(x, "docvars")[c("docname_", "docid_", "segid_")] <- temp
    attr(x, "names") <- temp[["docname_"]]
    return(x)
}

#' @noRd
#' @export
"docnames<-.dfm" <- function(x, value) {
    x <- as.dfm(x)
    temp <- make_docvars(length(value), value, TRUE)
    x@docvars[c("docname_", "docid_", "segid_")] <- temp
    x@Dimnames[["docs"]] <- temp[["docname_"]]
    return(x)
}

# names<- ----------------

#' @name names-quanteda
#' @title Special handling for names of quanteda objects
#' @description Keeps the element names and rownames in sync with the system docvar
#' `docname_`.
#' @inheritParams base::names
#' @method names<- corpus
#' @keywords internal corpus
#' @aliases names<-.corpus
#' @export
"names<-.corpus" <- function(x, value) {
    UseMethod("docnames<-")
}

#' @rdname names-quanteda
#' @aliases names<-.tokens
#' @method names<- tokens
#' @export
"names<-.tokens" <- function(x, value) {
    UseMethod("docnames<-")
}

setGeneric("rownames<-")

#' @include dfm-classes.R
#' @rdname names-quanteda
#' @aliases rownames<-.dfm
#' @export
setMethod("rownames<-",
          signature(x = "dfm"),
          function(x, value) {
              docnames(x) <- value
              return(x)
          })

#' @include fcm-classes.R
#' @rdname names-quanteda
#' @aliases rownames<-.fcm
#' @export
setMethod("rownames<-",
          signature(x = "fcm"),
          function(x, value) {
              set_fcm_dimnames(x) <- list(value, colnames(x))
              return(x)
          })

#' Internal function to extract docid
#' @rdname names-quanteda
#' @export
docid <- function(x) {
    get_docvars(x, "docid_", system = TRUE, drop = TRUE)
} 

