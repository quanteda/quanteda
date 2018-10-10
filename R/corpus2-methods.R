

#' @noRd
#' @export
texts.corpus2 <- function(x, groups = NULL, spacer = "  ") {
    txt <- as.character(unclass(x))
    
    # without groups
    if (is.null(groups)) {
        names(txt) <- docnames(x)
        return(txt)
    }
    
    if (is.character(groups) & all(groups %in% attr(x, "docvars"))) {
        if (any(is_internal(groups)))
            message_error("docvar_invalid")
        group <- as.factor(interaction(attr(x, "docvars")[groups]))
    } else {
        if (length(groups) != ndoc(x))
            stop("groups must name docvars or provide data matching the documents in x")
        group <- as.factor(groups)
    }
    
    texts(txt, groups = group, spacer = spacer)
}

#' @rdname texts
#' @param value character vector of the new texts
#' @return for \code{texts <-}, a corpus with the texts replaced by \code{value}
#' @export
#' @note You are strongly encouraged as a good practice of text analysis 
#'   workflow \emph{not} to modify the substance of the texts in a corpus. 
#'   Rather, this sort of processing is better performed through downstream 
#'   operations.  For instance, do not lowercase the texts in a corpus, or you 
#'   will never be able to recover the original case.  Rather, apply 
#'   \code{\link{tokens_tolower}} after applying \code{\link{tokens}} to a
#'   corpus, or use the option \code{tolower = TRUE} in \code{\link{dfm}}.
#' @examples
#' 
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @noRd
#' @export
"texts<-.corpus2" <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    return(x)
}

#' @rdname texts
#' @details \code{as.character(x)} where \code{x} is a corpus is equivalent to
#' calling \code{texts(x)}
#' @param ... unused
#' @method as.character corpus
#' @return \code{as.character(x)} is equivalent to \code{texts(x)}
#' @export
as.character.corpus2 <- function(x, ...) {
    texts(x)
}

#' @noRd
#' @export
docvars.corpus2 <- function(x, field = NULL) {
    result <- attr(x, "docvars")
    result <- result[!is_internal(names(result))]
    if (is.null(field)) {
        return(result)
    } else {
        return(result[field])
    }
}

#' @export
"docvars<-.corpus2" <- function(x, field = NULL, value) {
    if ("texts" %in% field) 
        stop("You should use texts() instead to replace the corpus texts.")
    if (any(is_internal(names(docvars))))
        message_error("docvar_invalid")
    attr(x, "docvars")[field] <- value
    x
}

#' @noRd
#' @export
"docnames<-.corpus2" <- function(x, value) {
    attr(x, "docvars")[["_docname"]] <- value
    return(x)
}

#' @noRd
#' @export
docnames.corpus2 <- function(x) {
    attr(x, "docvars")[["_docname"]]
}

is_internal <- function(x) {
    stri_startswith_fixed(x, "_")
}

#' @export
`+.corpus2` <- function(c1, c2) {

    result <- c(as.character(c1), as.character(c2))
    class(result) <- "corpus2"
    attr(result, "docvars") <- rbind(attr(c1, "docvars"), attr(c2, "docvars"))
    attr(result, "created") <- date()
    attr(result, "source") <- c("object" = "corpus2", 
                                Sys.info()["machine"], 
                                Sys.info()["user"])
    
    return(result)
}


#' @rdname corpus-class
#' @method [ corpus2
#' @export
#' @param i index for documents or rows of document variables
#' @examples 
#' 
#' corp <- corpus2(data_char_ukimmig2010)
#' corp[c("BNP", "Conservative", "PC")]
#' corp[c(1, 3, 7)]
#' 
#' corp <- as.corpus2(data_corpus_inaugural)
#' 
#' 
`[.corpus2` <- function(x, i) {
    
    attrs <- attributes(x)
    if (is.character(i)) {
        index <- fastmatch::fmatch(i, docnames(x))
    } else {
        index <- match(i, seq(length(x)))
    }
    is_na <- is.na(index)
    if (any(is_na))
        warning(paste(i[is_na], collapse = ", "), " do not exist")
    index <- index[!is_na]
    
    x <- as.character(unclass(x))[index]
    attrs$docvars <- attrs$docvars[index,,drop = FALSE] 
    attributes(x) <- attrs
    return(x)
}

#' @export
#' @rdname corpus-class
#' @method print corpus2
print.corpus2 <- function(x, ...) {
    print(stri_sub(x, 0, 100))
    print(attributes(x))
}

#' @export
#' @keywords internal
as.corpus2 <- function(x) {
    UseMethod("as.corpus2")
}

#' @export
as.corpus2.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.corpus2"))
}

#' @export
#' @method as.corpus2 corpus
as.corpus2.corpus <- function(x) {
    result <- corpus2(x$documents, "row.names", "texts")
    attr(result, "meta")$created <- as.POSIXct(x$metadata$created, 
                                               format = "%a %b %d %H:%M:%S %Y")
    return(result)
}

