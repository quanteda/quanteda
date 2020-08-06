#' Locate keywords-in-context
#'
#' For a text or a collection of texts (in a quanteda corpus object), return a
#' list of a keyword supplied by the user in its immediate context, identifying
#' the source text and the word index number within the source text.  (Not the
#' line number, since the text may or may not be segmented using end-of-line
#' delimiters.)
#' @param x a character, [corpus], or [tokens] object
#' @inheritParams pattern
#' @param window the number of context words to be displayed around the keyword.
#' @inheritParams valuetype
#' @param separator character to separate words in the output
#' @param ... additional arguments passed to [tokens], for applicable
#'   object types
#' @return A `kwic` classed data.frame, with the document name
#'   (`docname`), the token index positions (`from` and `to`,
#'   which will be the same for single-word patterns, or a sequence equal in
#'   length to the number of elements for multi-word phrases), the context
#'   before (`pre`), the keyword in its original format (`keyword`,
#'   preserving case and attached punctuation), and the context after
#'   (`post`).  The return object has its own `print` method, plus
#'   some special attributes that are hidden in the print view.  If you want to
#'   turn this into a simple data.frame, simply wrap the result in
#'   `data.frame`.
#'
#' @note `pattern` will be a keyword pattern or phrase, possibly multiple
#'   patterns, that may include punctuation.  If a pattern contains whitespace,
#'   it is best to wrap it in [phrase()] to make this explicit.
#'   However if `pattern` is a [collocations][textstat_collocations]
#'   or [dictionary] object, then the collocations or multi-word dictionary
#'   keys will automatically be considered phrases where each
#'   whitespace-separated element matches a token in sequence.
#' @export
#' @examples
#' head(kwic(data_corpus_inaugural, pattern = "secure*", window = 3, valuetype = "glob"))
#' head(kwic(data_corpus_inaugural, pattern = "secur", window = 3, valuetype = "regex"))
#' head(kwic(data_corpus_inaugural, pattern = "security", window = 3, valuetype = "fixed"))
#'
#' toks <- tokens(data_corpus_inaugural)
#' kwic(data_corpus_inaugural, pattern = phrase("war against"))
#' kwic(data_corpus_inaugural, pattern = phrase("war against"), valuetype = "regex")
#'
kwic <- function(x, pattern, window = 5,
                 valuetype = c("glob", "regex", "fixed"),
                 separator = " ",
                 case_insensitive = TRUE, ...) {
    UseMethod("kwic")
}

#' @export
kwic.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "kwic"))
}

#' @rdname kwic
#' @noRd
#' @export
kwic.character <- function(x, pattern, window = 5,
                           valuetype = c("glob", "regex", "fixed"),
                           separator = " ",
                           case_insensitive = TRUE, ...) {
    kwic(corpus(x), pattern, window, valuetype, separator, case_insensitive, ...)
}

#' @rdname kwic
#' @noRd
#' @export
kwic.corpus <- function(x, pattern, window = 5,
                        valuetype = c("glob", "regex", "fixed"),
                        separator = " ",
                        case_insensitive = TRUE, ...) {
    x <- as.corpus(x)
    kwic(tokens(x, what = "word", ...),
         pattern, window, valuetype, separator, case_insensitive)
}

#' @rdname kwic
#' @noRd
#' @examples
#' txt <- c("This is a test",
#'          "This is it.",
#'          "What is in a train?",
#'          "Is it a question?",
#'          "Sometimes you don't know if this is it.",
#'          "Is it a bird or a plane or is it a train?")
#' kwic(txt, c("is", "a"), valuetype = "fixed")
#' kwic(txt, phrase(c("is", "a", "is it")), valuetype = "fixed")
#'
#' toks <- tokens(txt)
#' kwic(toks, c("is", "a"), valuetype = "fixed")
#' kwic(toks, phrase(c("is", "a", "is it")), valuetype = "fixed")
#'
#' corp <- corpus(txt)
#' kwic(corp, c("is", "a"), valuetype = "fixed", separator = "", remove_separators = FALSE)
#' @export
kwic.tokens <- function(x, pattern, window = 5,
                        valuetype = c("glob", "regex", "fixed"),
                        separator = " ",
                        case_insensitive = TRUE, ...) {
    
    x <- as.tokens(x)
    if (is.list(pattern) && is.null(names(pattern)))
        names(pattern) <- pattern

    valuetype <- match.arg(valuetype)
    window <- as.integer(window)
    attrs <- attributes(x)
    ids <- pattern2list(pattern, attrs[["types"]], valuetype,
                        case_insensitive, field_object(attrs, "concatenator"))

    result <- qatd_cpp_kwic(x, attrs[["types"]], ids, seq_along(ids), window, separator)
    result[["pattern"]] <- factor(result[["pattern"]], levels = seq_along(ids),
                                  labels = names(ids))
    if (nrow(result))
        result <- result[order(match(result[["docname"]], docnames(x)),
                               result[["from"]],
                               result[["to"]],
                               result[["pattern"]]), ]
    rownames(result) <- NULL
    attr(result, "ntoken") <- ntoken(x)
    class(result) <- c("kwic", "data.frame")
    attributes(result, FALSE)  <- attributes(x)
    return(result)
}

#' @rdname kwic
#' @export
#' @examples
#' kw <- kwic(data_corpus_inaugural, "provident*")
#' is.kwic(kw)
#' is.kwic("Not a kwic")
#' is.kwic(kw[, c("pre", "post")])
is.kwic <- function(x) {
    inherits(x, "kwic")
}

#' @method print kwic
#' @importFrom stringi stri_c
#' @noRd
#' @export
print.kwic <- function(x, ...) {
    if (!nrow(x)) {
        cat("kwic object with 0 rows")
    } else if (!is.kwic(x)) {
        NextMethod()
    } else {
        if (all(x$from == x$to)) {
            labels <- stri_c("[", x$docname, ", ", x$from, "]")
        } else {
            labels <- stri_c("[", x$docname, ", ", x$from, ":", x$to, "]")
        }
        kwic <- data.frame(
            label = labels,
            pre = format(stri_replace_all_regex(x$pre, "(\\w*) (\\W)", "$1$2"), justify = "right"),
            s1 = rep("|", nrow(x)),
            keyword = format(x$keyword, justify = "centre"),
            s2 = rep("|", nrow(x)),
            post = format(stri_replace_all_regex(x$post, "(\\w*) (\\W)", "$1$2"), justify = "left")
        )
        colnames(kwic) <- NULL
        print(kwic, row.names = FALSE)
    }
}

#' @method "[" kwic
#' @export
#' @noRd
"[.kwic" <- function(x, i, j, ...) {
    if (!missing(j))
        x <- as.data.frame(x)
    NextMethod("[")
}
