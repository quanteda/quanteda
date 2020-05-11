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

#' @rdname print-quanteda
#' @method print tokens
#' @param max_ntoken max number of tokens to print; default is from the
#'   `print_tokens_max_ntoken` setting of [quanteda_options()]
#' @param ... not used
#' @export
print.tokens <- function(x, max_ndoc = quanteda_options("print_tokens_max_ndoc"),
                         max_ntoken = quanteda_options("print_tokens_max_ntoken"),
                         show_summary = quanteda_options("print_tokens_summary"),
                         ...) {
    unused_dots(...)
    docvars <- docvars(x)
    ndoc <- ndoc(x)
    if (max_ndoc < 0)
        max_ndoc <- ndoc(x)

    if (show_summary) {
        cat("Tokens consisting of ", format(ndoc, big.mark = ","), " document",
            if (ndoc > 1L) "s" else "", sep = "")
        if (ncol(docvars))
            cat(" and ", format(ncol(docvars), big.mark = ","), " docvar",
                if (ncol(docvars) == 1L) "" else "s", sep = "")
        cat(".\n")
    }

    if (max_ndoc > 0) {
        x <- head(x, max_ndoc)
        label <- paste0(names(x), " :")
        types <- c("", types(x))
        len <- lengths(x)
        if (max_ntoken < 0)
            max_ntoken <- max(len)
        x <- lapply(unclass(x), function(y) types[head(y, max_ntoken) + 1]) # shift index to show padding
        for (i in seq_along(label)) {
            cat(label[i], "\n", sep = "")
            print(x[[i]])
            if (len[i] > max_ntoken)
                cat("[ ... and ",  format(len[i] - max_ntoken, big.mark = ","), " more ]\n", sep = "")
            cat("\n", sep = "")
        }
        ndoc_rem <- ndoc - max_ndoc
        if (ndoc_rem > 0)
            cat("[ reached max_ndoc ... ", format(ndoc_rem, big.mark = ","), " more document",
                if (ndoc_rem > 1) "s", " ]\n", sep = "")
    }
}


#' @method "[" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[c(1,3)]
"[.tokens" <- function(x, i) {

    if (missing(i)) return(x)
    x <- as.tokens(x)
    attrs <- attributes(x)

    index <- seq_along(docnames(x))
    names(index) <- docnames(x)
    index <- index[i]
    if (any(is.na(index)))
        stop("Subscript out of bounds")

    result <- build_tokens(
        unclass(x)[index],
        attrs[["types"]],
        docvars = reshape_docvars(attrs[["docvars"]], index),
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
    tokens_recompile(result)
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
    attrs2 <- attributes(t2)
    attrs1 <- attributes(t1)

    if (length(intersect(docnames(t1), docnames(t2))))
        stop("Cannot combine tokens with duplicated document names", call. = FALSE)
    if (!identical(field_object(attrs1, "what"), field_object(attrs2, "what")))
        stop("Cannot combine tokens in different tokenization units", call. = FALSE)
    if (!identical(field_object(attrs1, "concatenator"), field_object(attrs2, "concatenator")))
        stop("Cannot combine tokens with different concatenators", call. = FALSE)

    docvars <- rbind_fill(get_docvars(t1, user = TRUE, system = TRUE),
                          get_docvars(t2, user = TRUE, system = TRUE))
    t2 <- unclass(t2)
    t1 <- unclass(t1)
    t2 <- lapply(t2, function(x, y) x + (y * (x != 0)),
                 length(attrs1[["types"]])) # shift non-zero IDs
    result <- build_tokens(
        c(t1, t2),
        c(attrs1[["types"]], attrs2[["types"]]),
        what = field_object(attrs1, "what"),
        ngram = sort(unique(c(
            field_object(attrs1, "ngram"),
            field_object(attrs2, "ngram")))
            ),
        skip = sort(unique(c(
            field_object(attrs1, "skip"),
            field_object(attrs2, "skip")))
            ),
        concatenator = field_object(attrs1, "concatenator"),
        docvars = docvars,
        class = attrs1[["class"]]
    )
    tokens_recompile(result)
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
