#' Base method extensions for tokens objects
#'
#' Extensions of base R functions for tokens objects.
#' @name tokens-class
#' @param x a tokens object
#' @keywords internal tokens
NULL

#' @rdname as.tokens
#' @return `as.list` returns a simple list of characters from a
#'   [tokens] object.
#' @method as.list tokens
#' @export
as.list.tokens <- function(x, ...) {
    types <- c("", get_types(x))
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

#' @rdname tokens-class
#' @return `unlist` returns a simple vector of characters from a
#'   [tokens] object.
#' @param recursive a required argument for [unlist] but inapplicable to
#'   [tokens] objects.
#' @method unlist tokens
#' @keywords internal
#' @export
unlist.tokens <- function(x, recursive = FALSE, use.names = TRUE) {
    unlist(as.list(x), use.names = use.names)
}

#' @rdname print-methods
#' @method print tokens
#' @param max_ntoken max number of tokens to print; default is from the
#'   `print_tokens_max_ntoken` setting of [quanteda_options()].
#' @export
print.tokens <- function(x, max_ndoc = quanteda_options("print_tokens_max_ndoc"),
                         max_ntoken = quanteda_options("print_tokens_max_ntoken"),
                         show_summary = quanteda_options("print_tokens_summary"),
                         ...) {
    
    max_ndoc <- check_integer(max_ndoc, min = -1)
    max_ntoken <- check_integer(max_ntoken, min = -1)
    show_summary <- check_logical(show_summary)
    
    docvars <- docvars(x)
    ndoc <- ndoc(x)
    if (max_ndoc < 0)
        max_ndoc <- ndoc(x)

    if (show_summary) {
        cat("Tokens consisting of ", format(ndoc, big.mark = ","), " document",
            if (ndoc != 1L) "s" else "", sep = "")
        if (ncol(docvars))
            cat(" and ", format(ncol(docvars), big.mark = ","), " docvar",
                if (ncol(docvars) != 1L) "s" else "", sep = "")
        if (is.tokens_xptr(x))
            cat(" (pointer to ", address(x), ")", sep = "")
        cat(".\n")
    }

    if (max_ndoc > 0 && ndoc(x) > 0) {
        x <- head(as.tokens(x), max_ndoc)
        label <- paste0(names(x), " :")
        types <- c("", get_types(x))
        len <- lengths(x)
        if (max_ntoken < 0)
            max_ntoken <- max(len)
        x <- lapply(unclass(x), function(y) types[head(y, max_ntoken) + 1]) # shift index to show padding
        for (i in seq_along(label)) {
            cat(label[i], "\n", sep = "")
            print(x[[i]], ...)
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


#' @rdname tokens-class
#' @method [ tokens
#' @param i document names or indices for documents to extract.
#' @param drop_docid if `TRUE`, `docid` for documents are removed as the result
#'   of extraction.
#' @export
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[c(1,3)]
"[.tokens" <- function(x, i, drop_docid = TRUE) {

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
        docvars = reshape_docvars(attrs[["docvars"]], index, drop_docid = drop_docid),
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
    tokens_recompile(result)
}

#' @rdname tokens-class
#' @method [[ tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[[2]]
"[[.tokens" <- function(x, i) {
    unlist_character(as.list(x[head(i, 1)]), use.names = FALSE)
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

#' @rdname tokens-class
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
    # NOTE: consider deprecating
    c(t1, t2)
}

#' @rdname tokens-class
#' @export
c.tokens_xptr <- function(...) {
    
    x <- list(...)
    
    if (!all(unlist(lapply(x, is.tokens_xptr)))) 
        stop("Cannot combine different types of objects", call. = FALSE)
    
    if (any(duplicated(unlist(lapply(x, docnames)))))
        stop("Cannot combine tokens with duplicated document names", call. = FALSE)
 
    docvars <- lapply(x, function(x) get_docvars(x, user = TRUE, system = TRUE))
    attrs <- lapply(x, attributes)
    what <- unlist(lapply(attrs, field_object, "what"))
    conct <- unlist(lapply(attrs, field_object, "concatenator"))
    
    if (length(unique(what))> 1)
        stop("Cannot combine tokens in different tokenization units", call. = FALSE)
    if (length(unique(conct))> 1)
        stop("Cannot combine tokens with different concatenators", call. = FALSE)
    
    ngram <- unlist(lapply(attrs, field_object, "ngram"))
    skip <- unlist(lapply(attrs, field_object, "skip"))
    
    temp <- combine_tokens(...)
    cpp_recompile(temp)
    
    build_tokens(
        temp, types = NULL,
        what = field_object(attrs[[1]], "what"),
        tokenizer = field_object(attrs[[1]], "tokenizer"),
        ngram = sort(unique(ngram)),
        skip = sort(unique(skip)),
        concatenator = field_object(attrs[[1]], "concatenator"),
        docvars = do.call(combine_docvars, docvars),
        class = attrs[[1]][["class"]]
    )
}

#' @rdname tokens-class
#' @export
c.tokens <- function(...) {
    
    x <- list(...)
    
    if (!all(unlist(lapply(x, is.tokens)))) 
        stop("Cannot combine different types of objects", call. = FALSE)
    
    as.tokens(do.call(c, lapply(x, as.tokens_xptr)))
}

combine_tokens <- function(...) {
    x <- list(...)
    if (length(x) == 1) 
        return(x[[1]])
    result <- cpp_tokens_combine(x[[1]], x[[2]], get_threads())
    if (length(x) == 2) return(result)
    for (i in seq(3, length(x)))
        result <- combine_tokens(result, x[[i]])
    return(result)
}

combine_docvars <- function(...) {
    x <- list(...)
    if (length(x) == 1) 
        return(x[[1]])
    result <- rbind_fill(x[[1]], x[[2]])
    if (length(x) == 2) return(result)
    for (i in seq(3, length(x)))
        result <- combine_docvars(result, x[[i]])
    return(result)
}

get_concatenator <- function(x) {
    attr(x, "meta")$object$concatenator
}

"set_concatenator<-" <- function(x, value) {
    if (!is.character(value) || length(value) != 1L)
        stop("concatenator value must be a single character")
    attr(x, "meta")$object$concatenator <- value
    return(x)
}
