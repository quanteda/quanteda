#' Locate keywords-in-context
#'
#' For a text or a collection of texts (in a quanteda corpus object), return a
#' list of a keyword supplied by the user in its immediate context, identifying
#' the source text and the word index number within the source text.  (Not the
#' line number, since the text may or may not be segmented using end-of-line
#' delimiters.)
#' @param x a character, [corpus], or [tokens] object
#' @inheritParams pattern
#' @param window the number of context words to be displayed around the keyword
#' @inheritParams valuetype
#' @param separator a character to separate words in the output
#' @param index an [index] object to specify keywords
#' @param ... unused
#' @return A `kwic` classed data.frame, with the document name
#'   (`docname`) and the token index positions (`from` and `to`,
#'   which will be the same for single-word patterns, or a sequence equal in
#'   length to the number of elements for multi-word phrases).
#'   
#' @note `pattern` will be a keyword pattern or phrase, possibly multiple
#'   patterns, that may include punctuation.  If a pattern contains whitespace,
#'   it is best to wrap it in [phrase()] to make this explicit. However if
#'   `pattern` is a `collocations` (see \pkg{quanteda.textstats} or
#'   [dictionary] object, then the collocations or multi-word dictionary keys
#'   will automatically be considered phrases where each whitespace-separated
#'   element matches a token in sequence.
#' @export
#' @seealso [print-methods]
#' @examples
#' # single token matching
#' toks <- tokens(data_corpus_inaugural[1:8])
#' kwic(toks, pattern = "secure*", valuetype = "glob", window = 3)
#' kwic(toks, pattern = "secur", valuetype = "regex", window = 3)
#' kwic(toks, pattern = "security", valuetype = "fixed", window = 3)
#'
#' # phrase matching
#' kwic(toks, pattern = phrase("secur* against"), window = 2)
#' kwic(toks, pattern = phrase("war against"), valuetype = "regex", window = 2)
#' 
#' # use index
#' idx <- index(toks, phrase("secur* against"))
#' kwic(toks, index = idx, window = 2)
kwic <- function(x, pattern, window = 5,
                 valuetype = c("glob", "regex", "fixed"),
                 separator = " ",
                 case_insensitive = TRUE, 
                 index = NULL, 
                 ...) {
    UseMethod("kwic")
}

#' @export
kwic.default <- function(x, ...) {
    check_class(class(x), "kwic")
}

#' @export
kwic.character <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "kwic.character()",
        details = 'Please apply `tokens()` to the character object first.'
    )
}

#' @export
kwic.corpus <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "kwic.corpus()",
        details = 'Please apply `tokens()` to the corpus object first.'
    )
}

#' @export
kwic.tokens_xptr <- function(x, pattern = NULL, window = 5,
                        valuetype = c("glob", "regex", "fixed"),
                        separator = " ",
                        case_insensitive = TRUE, 
                        index = NULL,
                        ...) {
    
    check_dots(..., "kwic")
    
    window <- check_integer(window, 1, 1, 0)
    valuetype <- match.arg(valuetype)
    separator <- check_character(separator)
    case_insensitive <- check_logical(case_insensitive)
    
    if (is.null(pattern) && is.null(index))
        stop("Either pattten or index must be provided\n", call. = FALSE)
    if (!is.null(pattern)) {
        index <- index(x, pattern = pattern, valuetype = valuetype, 
                        case_insensitive = case_insensitive)
    } else if (!is.null(index)) {
        if (!is.index(index))
            stop("Invalid index object\n", call. = FALSE)
    }
    
    n <- ntoken(x)
    index$document <- match(index$docname, docnames(x))
    index <- subset(index, !is.na(index$document))
    result <- cbind(index, cpp_kwic(x, index$document, index$from, index$to, 
                                    window, separator, get_threads()))

    # reorder columns to match pre-v3 order
    result <- result[, c("docname", "from", "to", "pre", "keyword", "post", "pattern")]
    attr(result, "ntoken") <- n[unique(index$docname)]
    class(result) <- c("kwic", "data.frame")
    rownames(result) <- NULL
    return(result)
}

#' @export
kwic.tokens <- function(x, ...) {
    kwic(as.tokens_xptr(x), ...)
}

#' @rdname kwic
#' @export
#' @examples
#' kw <- kwic(tokens(data_corpus_inaugural[1:20]), "provident*")
#' is.kwic(kw)
#' is.kwic("Not a kwic")
#' is.kwic(kw[, c("pre", "post")])
#' 
is.kwic <- function(x) {
    inherits(x, "kwic")
}

#' @rdname kwic
#' @method as.data.frame kwic
#' @examples
#' toks <- tokens(data_corpus_inaugural[1:8])
#' kw <- kwic(toks, pattern = "secure*", valuetype = "glob", window = 3)
#' as.data.frame(kw)
#' 
#' @export
as.data.frame.kwic <- function(x, ...) {
    attr(x, "ntoken") <- NULL
    class(x) <- "data.frame"
    return(x)
}

#' @rdname print-methods
#' @method print kwic
#' @param max_nrow max number of documents to print; default is from the
#'   `print_kwic_max_nrow` setting of [quanteda_options()]
#' @importFrom stringi stri_c stri_c_list
#' @export
print.kwic <- function(x, max_nrow = quanteda_options("print_kwic_max_nrow"), 
                       show_summary = quanteda_options("print_kwic_summary"), ...) {

    max_nrow <- check_integer(max_nrow, min = -1)
    if (max_nrow < 0)
        max_nrow <- nrow(x)
    nrem <- max(0, nrow(x) - max_nrow)
    
    x <- head(x, max_nrow)
    if (show_summary) {
        cat(msg("Keyword-in-context with %d %s",
                list(nrow(x) + nrem, c("match", "matches")),
                list(1, nrow(x) + nrem != 1)), ".", sep = "")
    }
    
    if (nrow(x)) {
        if (all(x$from == x$to)) {
            labels <- stri_c("[", x$docname, ", ", x$from, "]")
        } else {
            labels <- stri_c("[", x$docname, ", ", x$from, ":", x$to, "]")
        }
        result <- data.frame(
            label = labels,
            pre = format(stri_replace_all_regex(x$pre, "(\\p{L}*) (\\p{Po})", "$1$2"), justify = "right"),
            s1 = rep("|", nrow(x)),
            keyword = format(x$keyword, justify = "centre"),
            s2 = rep("|", nrow(x)),
            post = format(stri_replace_all_regex(x$post, "(\\p{L}*) (\\p{Po})", "$1$2"), justify = "left")
        )
        colnames(result) <- NULL
        print(result, row.names = FALSE)

        if (nrem > 0) {
            cat("[", sep = "") 
            if (nrem > 0) {
                cat(" reached max_nrow ... ", format(nrem, big.mark = ","), " more match", sep = "") 
                if (nrem > 1) cat("es", sep = "")
            }
            cat(" ]\n", sep = "") 
        }
    }
    cat("\n", sep = "") 
}

#' @method [ kwic
#' @export
#' @noRd
`[.kwic` <- function(x, i, j, ...) {
    attrs <- attributes(x)
    class(x) <- c("data.frame")
    x <- x[i,]
    attr(x, "ntoken") <- attrs$ntoken[i]
    class(x) <- c("kwic", "data.frame")
    rownames(x) <- NULL
    return(x)
}
