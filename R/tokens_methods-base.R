#' @rdname as.tokens
#' @return \code{as.list} returns a simple list of characters from a
#'   \link{tokens} object.
#' @method as.list tokens
#' @export
as.list.tokens <- function(x, ...) {
    types <- c("", types(x))
    result <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding
    attributes(result) <- NULL
    names(result) <- names(x)
    return(result)
}

#' @rdname as.tokens
#' @param use.names logical; preserve names if \code{TRUE}.  For
#'   \code{as.character} and \code{unlist} only.
#' @return \code{as.character} returns a character vector from a
#'   \link{tokens} object.
#' @export
as.character.tokens <- function(x, use.names = FALSE, ...) {
    unlist(as.list(x), use.names = use.names)
}

#' @rdname as.tokens
#' @export
#' @return \code{is.tokens} returns \code{TRUE} if the object is of class
#'   tokens, \code{FALSE} otherwise.
is.tokens <- function(x) "tokens" %in% class(x)

# extension of generics for tokens -----------

#' @rdname as.tokens
#' @return \code{unlist} returns a simple vector of characters from a
#'   \link{tokens} object.
#' @param recursive a required argument for \link{unlist} but inapplicable to
#'   \link{tokens} objects
#' @method unlist tokens
#' @export
unlist.tokens <- function(x, recursive = FALSE, use.names = TRUE) {
    unlist(as.list(x), use.names = use.names)
}

#' print a tokens objects
#' print method for a tokens object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokens
#' @noRd
print.tokens <- function(x, ...) {
    cat(class(x)[1], " from ", ndoc(x), " document",
        if (ndoc(x) > 1L) "s" else "", ".\n", sep = "")
    types <- c("", types(x))
    x <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding
    class(x) <- "listof"
    print(x, ...)
}


#' @method "[" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[c(1,3)]
"[.tokens" <- function(x, i) {
    
    # if (length(x) == 1 && is.null(x[[1]])) return(x)
    # 
    # error <- FALSE
    # if (is.character(i) && any(!i %in% names(x))) error <- TRUE
    # if (is.numeric(i) && any(i > length(x))) error <- TRUE
    # if (error) stop("Subscript out of bounds")
    # 
    # attrs <- attributes(x)
    # x <- unclass(x)[i]
    # attrs$docvars <- reshape_docvars(attrs$docvars, i)
    # attributes(x, FALSE) <- attrs
    x <- as.tokens(x)
    attrs <- attributes(x)
    if (is.character(i)) {
        index <- fmatch(i, docnames(x))
    } else if (is.numeric(i)) {
        index <- match(i, seq_len(length(x)))
    } else {
        index <- which(i)
    }
    is_na <- is.na(index)
    if (any(is_na))
        warning(paste(i[is_na], collapse = ", "), " do not exist")
    index <- index[!is_na]
    
    x <- unclass(x)[index]
    attrs$docvars <- reshape_docvars(attrs$docvars, index)
    attributes(x, FALSE) <- attrs
    tokens_recompile(x)
}

#' @method "[[" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[[2]]
"[[.tokens" <- function(x, i) {
    types <- c("", types(x))
    types[unclass(x)[[i]] + 1] # shift index to show padding
}

#' @method "$" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks$d3
"$.tokens" <- function(x, i, ...) {
    x[[i]]
}

#' @method "[<-" tokens
#' @export
#' @noRd
"[<-.tokens" <- function(x, i, value) {
    stop("assignment to tokens objects is not allowed", call. = FALSE)
}

#' @method "[[<-" tokens
#' @export
#' @noRd
"[[<-.tokens" <- function(x, i, value) {
    stop("assignment to tokens objects is not allowed", call. = FALSE)
}

#' @method lengths tokens
#' @noRd
#' @export
lengths.tokens <- function(x, use.names = TRUE) {
    NextMethod()
}

#' @rdname as.tokens
#' @param t1 tokens one to be added
#' @param t2 tokens two to be added
#' @return \code{c(...)} and \code{+} return a tokens object whose documents
#'   have been added as a single sequence of documents.
#' @examples
#' # combining tokens
#' toks1 <- tokens(c(doc1 = "a b c d e", doc2 = "f g h"))
#' toks2 <- tokens(c(doc3 = "1 2 3"))
#' toks1 + toks2
#' c(toks1, toks2)
#'
#' @export
`+.tokens` <- function(t1, t2) {
    if (length(intersect(docnames(t1), docnames(t2))))
        stop("Cannot combine tokens with duplicated document names")
    if (!identical(attr(t1, "what"), attr(t2, "what")))
        stop("Cannot combine tokens in different units")
    if (!identical(attr(t1, "concatenator"), attr(t2, "concatenator")))
        stop("Cannot combine tokens with different concatenators")
    
    attrs <- list(what = attr(t1, "what"),
                  ngrams = sort(unique(c(attr(t1, "ngrams"), attr(t2, "ngrams")))),
                  skip = sort(unique(c(attr(t1, "skip"), attr(t2, "skip")))),
                  concatenator = attr(t1, "concatenator"),
                  docvars = data.frame(row.names = c(docnames(t1), docnames(t2))))
    
    docvars(t1) <- docvars(t2) <- NULL
    types2 <- types(t2)
    types1 <- types(t1)
    t2 <- unclass(t2)
    t1 <- unclass(t1)
    t2 <- lapply(t2, function(x, y) x + y, length(types1)) # shift IDs
    t1 <- c(t1, t2)
    class(t1) <- "tokens"
    types(t1) <- c(types1, types2)
    t1 <- tokens_recompile(t1)
    attributes(t1, FALSE) <- attrs
    return(t1)
}

#' @rdname as.tokens
#' @export
c.tokens <- function(...) {
    x <- list(...)
    if (length(x) == 1) return(x[[1]])
    result <- x[[1]] + x[[2]]
    if (length(x) == 2) return(result)
    for (i in seq(3, length(x)))
        result <- result + x[[i]]
    return(result)
}
