#' @rdname as.tokens
#' @return `as.list` returns a simple list of characters from a
#'   [tokens] object.
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
#' @param use.names logical; preserve names if `TRUE`.  For
#'   `as.character` and `unlist` only.
#' @return `as.character` returns a character vector from a
#'   [tokens] object.
#' @export
as.character.tokens <- function(x, use.names = FALSE, ...) {
    unlist(as.list(x), use.names = use.names)
}

#' @rdname as.tokens
#' @export
#' @return `is.tokens` returns `TRUE` if the object is of class
#'   tokens, `FALSE` otherwise.
is.tokens <- function(x) "tokens" %in% class(x)

# extension of generics for tokens -----------

#' @rdname as.tokens
#' @return `unlist` returns a simple vector of characters from a
#'   [tokens] object.
#' @param recursive a required argument for [unlist] but inapplicable to
#'   [tokens] objects
#' @method unlist tokens
#' @export
unlist.tokens <- function(x, recursive = FALSE, use.names = TRUE) {
    unlist(as.list(x), use.names = use.names)
}

#' print a tokens objects
#' print method for a tokens object
#' @param x a tokens object created by [tokens()]
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
    
    x <- as.tokens(x)
    attrs <- attributes(x)
    
    index <- seq_along(docnames(x))
    names(index) <- docnames(x)
    index <- index[i]
    if (any(is.na(index)))
        stop("Subscript out of bounds")
    x <- unclass(x)[index]
    attrs$docvars <- subset_docvars(attrs$docvars, index)
    attrs$names <- attrs$docvars[["docname_"]]
    attributes(x) <- attrs
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
#' @return `c(...)` and `+` return a tokens object whose documents
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
    
    t2 <- as.tokens(t2)
    t1 <- as.tokens(t1)
    
    if (length(intersect(docnames(t1), docnames(t2))))
        stop("Cannot combine tokens with duplicated document names", call. = FALSE)
    if (!identical(attr(t1, "what"), attr(t2, "what")))
        stop("Cannot combine tokens in different tokenization units", call. = FALSE)
    if (!identical(attr(t1, "concatenator"), attr(t2, "concatenator")))
        stop("Cannot combine tokens with different concatenators", call. = FALSE)
    
    docvar <- rbind_fill(get_docvars(t1, user = TRUE, system = TRUE), 
                         get_docvars(t2, user = TRUE, system = TRUE))
    attrs2 <- attributes(t2)
    attrs1 <- attributes(t1)
    t2 <- unclass(t2)
    t1 <- unclass(t1)
    t2 <- lapply(t2, function(x, y) x + (y * (x != 0)), 
                 length(attrs1$types)) # shift non-zero IDs
    result <- compile_tokens(
        c(t1, t2), docvar[["docname_"]],
        what = attr(t1, "what"),
        ngrams = sort(unique(c(attrs1$ngrams, attrs2$ngrams))),
        skip = sort(unique(c(attrs1$skip, attrs2$skip))),
        concatenator = attrs1$concatenator,
        types = c(attrs1$types, attrs2$types),
        docvars = docvar
    )
    result <- tokens_recompile(result)
    return(result)
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
