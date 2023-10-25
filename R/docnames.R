# docnames -----------

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
#' toks <- tokens(corp)
#' docnames(toks) <- char_tolower(docnames(toks))
#' 
#' # get and set doument names to a dfm
#' dfmat <- dfm(tokens(corp))
#' docnames(dfmat) <- char_tolower(docnames(dfmat))
#' 
#' @keywords corpus dfm
docnames <- function(x) {
    UseMethod("docnames")
}

#' @export
docnames.default <- function(x) {
    check_class(class(x), "docnames")
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

# docnames<- ----------

#' @param value a character vector of the same length as `x`
#' @return `docnames <-` assigns new values to the document names of an object.  
#' docnames can only be character, so any non-character value assigned to be a
#' docname will be coerced to mode `character`.
#' @export
#' @examples 
#' # reassign the document names of the inaugural speech corpus
#' corp <- data_corpus_inaugural
#' docnames(corp) <- paste0("Speech", seq_len(ndoc(corp)))
#' 
#' @rdname docnames
"docnames<-" <- function(x, value) {
    UseMethod("docnames<-")
}

#' @export
"docnames<-.default" <- function(x, value) {
    check_class(class(x), "docnames<-")
}

#' @noRd
#' @export
"docnames<-.corpus" <- function(x, value) {
    x <- as.corpus(x)
    temp <- make_docvars(length(value), value, unique = TRUE)
    attr(x, "docvars")[c("docname_", "docid_", "segid_")] <- temp
    attr(x, "names") <- temp[["docname_"]]
    return(x)
}

#' @noRd
#' @export
"docnames<-.tokens" <- function(x, value) {
    x <- as.tokens(x)
    temp <- make_docvars(length(value), value, unique = TRUE)
    attr(x, "docvars")[c("docname_", "docid_", "segid_")] <- temp
    attr(x, "names") <- temp[["docname_"]]
    return(x)
}

#' @noRd
#' @export
"docnames<-.dfm" <- function(x, value) {
    x <- as.dfm(x)
    temp <- make_docvars(length(value), value, unique = TRUE)
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

# docid -------------

#' @rdname docnames
#' @return `docid` returns an internal variable denoting the original "docname"
#'   from which a document came.  If an object has been reshaped (e.g.
#'   [corpus_reshape()] or segmented (e.g. [corpus_segment()]), `docid(x)` returns 
#'   the original docnames but `segid(x)` does the serial number of those segments 
#'   within the original document. 
#' @note `docid` and `segid` are designed primarily for developers, not for end users.  In
#'   most cases, you will want `docnames` instead.  It is, however, the
#'   default for [groups], so that documents that have been previously reshaped
#'   (e.g. [corpus_reshape()] or segmented (e.g.
#'   [corpus_segment()]) will be regrouped into their original `docnames` when
#'   `groups = docid(x)`.
#' @export
#' @examples 
#' 
#' corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.",
#'                  textwo = "Sentence 1. Sentence 2."))
#' corp_sent <- corp |>
#'     corpus_reshape(to = "sentences")
#' docnames(corp_sent)
#' 
#' # docid
#' docid(corp_sent)
#' docid(tokens(corp_sent))
#' docid(dfm(tokens(corp_sent)))
#' 
#' # segid
#' segid(corp_sent)
#' segid(tokens(corp_sent))
#' segid(dfm(tokens(corp_sent)))
docid <- function(x) {
    UseMethod("docid")
}

#' @export
docid.default <- function(x) {
    check_class(class(x), "docid")
}

#' @export
docid.corpus <- function(x) {
    tryCatch({
        return(get_docvars(x, "docid_", system = TRUE, drop = TRUE))
    }, error = function(e) {
        return(NULL)
    })
}

#' @export
docid.tokens <- function(x) {
    tryCatch({
        return(get_docvars(x, "docid_", system = TRUE, drop = TRUE))
    }, error = function(e) {
        return(NULL)
    })
}

#' @export
docid.dfm <- function(x) {
    tryCatch({
        return(get_docvars(x, "docid_", system = TRUE, drop = TRUE))
    }, error = function(e) {
        return(NULL)
    })
}

#' @rdname docnames
#' @export
segid <- function(x) {
    UseMethod("segid")
}

#' @export
segid.default <- function(x) {
    check_class(class(x), "segid")
}

#' @export
segid.corpus <- function(x) {
    get_docvars(x, "segid_", system = TRUE, drop = TRUE)
}

#' @export
segid.tokens <- function(x) {
    get_docvars(x, "segid_", system = TRUE, drop = TRUE)
}

#' @export
segid.dfm <- function(x) {
    get_docvars(x, "segid_", system = TRUE, drop = TRUE)
}
