#' Locate keywords-in-context
#'
#' For a text or a collection of texts (in a quanteda corpus object), return a
#' list of a keyword supplied by the user in its immediate context, identifying
#' the source text and the word index number within the source text.  (Not the
#' line number, since the text may or may not be segmented using end-of-line
#' delimiters.)
#' @param x a character, \link{corpus}, or \link{tokens} object
#' @inheritParams pattern
#' @param window the number of context words to be displayed around the keyword.
#' @inheritParams valuetype
#' @param case_insensitive match without respect to case if \code{TRUE}
#' @param separator character to separate words in the output
#' @param ... additional arguments passed to \link{tokens}, for applicable
#'   object types
#' @return A \code{kwic} classed data.frame, with the document name
#'   (\code{docname}), the token index positions (\code{from} and \code{to},
#'   which will be the same for single-word patterns, or a sequence equal in
#'   length to the number of elements for multi-word phrases), the context
#'   before (\code{pre}), the keyword in its original format (\code{keyword},
#'   preserving case and attached punctuation), and the context after
#'   (\code{post}).  The return object has its own \code{print} method, plus
#'   some special attributes that are hidden in the print view.  If you want to
#'   turn this into a simple data.frame, simply wrap the result in
#'   \code{data.frame}.
#'
#' @note \code{pattern} will be a keyword pattern or phrase, possibly multiple
#'   patterns, that may include punctuation.  If a pattern contains whitespace,
#'   it is best to wrap it in \code{\link{phrase}} to make this explicit.
#'   However if \code{pattern} is a \link[=textstat_collocations]{collocations}
#'   or \link{dictionary} object, then the collocations or multi-word dictionary
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

    if (is.collocations(pattern) || is.dictionary(pattern))
        pattern <- phrase(pattern)
    kwic(tokens(x, ...),
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

    if (is.list(pattern) && is.null(names(pattern)))
        names(pattern) <- pattern

    valuetype <- match.arg(valuetype)
    type <- types(x)
    
    # add document names if none (TODO: should be removed after corpus upgrade)
    if (is.null(names(x)))
        names(x) <- paste0(quanteda_options("base_docname"), seq_len(x))
    
    ids <- pattern2list(pattern, type,
                        valuetype, case_insensitive, attr(x, "concatenator"))
    result <- data.frame()
    for (m in unique(names(ids))) {
        temp <- qatd_cpp_kwic(x, type, ids[names(ids) == m], window, separator)
        temp[["pattern"]] <- rep(m, nrow(temp))
        result <- rbind(result, temp)
    }
    result[["pattern"]] <- factor(result[["pattern"]], levels = unique(names(ids)))
    if (nrow(result))
        result <- result[order(match(result[["docname"]], docnames(x)), 
                               result[["from"]], result[["to"]]),]
    rownames(result) <- NULL
    attr(result, "ntoken") <- ntoken(x)
    class(result) = c("kwic", "data.frame")
    attributes(result, FALSE)  <- attributes(x)
    return(result)
}

#' @rdname kwic
#' @export
#' @examples
#' kw <- kwic(data_corpus_inaugural, "provident*")
#' is.kwic(kw)
#' is.kwic("Not a kwic")
is.kwic <- function(x) "kwic" %in% class(x)

#' @method print kwic
#' @noRd
#' @export
print.kwic <- function(x, ...) {
    if (!nrow(x)) {
        cat("kwic object with 0 rows")
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
