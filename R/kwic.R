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
#' @param separator character to separate words in the output
#' @param ... additional arguments passed to [tokens], for applicable
#'   object types
#' @return A `kwic` classed data.frame, with the document name
#'   (`docname`) and the token index positions (`from` and `to`,
#'   which will be the same for single-word patterns, or a sequence equal in
#'   length to the number of elements for multi-word phrases).
#'
#' @note `pattern` will be a keyword pattern or phrase, possibly multiple
#'   patterns, that may include punctuation.  If a pattern contains whitespace,
#'   it is best to wrap it in [phrase()] to make this explicit. However if
#'   `pattern` is a [collocations][quanteda.textstats::textstat_collocations] or
#'   [dictionary] object, then the collocations or multi-word dictionary keys
#'   will automatically be considered phrases where each whitespace-separated
#'   element matches a token in sequence.
#' @export
#' @examples
#' corp <- data_corpus_inaugural
#' print(head(kwic(corp, pattern = "secure*", valuetype = "glob")), window = 3)
#' print(head(kwic(corp, pattern = "secur", valuetype = "regex")), window = 3)
#' print(head(kwic(corp, pattern = "security", valuetype = "fixed")), window = 3)
#'
#' toks <- tokens(corp)
#' print(kwic(toks, pattern = phrase("war against")))
#' print(kwic(toks, pattern = phrase("war against"), valuetype = "regex"))
#'
kwic <- function(x, pattern, window = 5,
                 valuetype = c("glob", "regex", "fixed"),
                 separator = " ",
                 case_insensitive = TRUE, ...) {
    UseMethod("kwic")
}

#' @export
kwic.default <- function(x, ...) {
    check_class(class(x), "kwic")
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
    kwic(tokens(x, what = "word", ...), pattern, 
         window, valuetype, separator, case_insensitive, ...)
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
#' kwic(txt, "is")
#' kwic(txt, "in", valuetype = "regex")
#' kwic(txt, phrase(c("is", "a", "is it")), valuetype = "fixed")
#'
#' toks <- tokens(txt)
#' kwic(txt, "is", valuetype = "regex")
#' kwic(txt, "or", window = 2)
#'
#' corp <- corpus(txt)
#' print(kwic(corp, "is"), separator = "")
#' @export
kwic.tokens <- function(x, pattern, window = 5,
                           valuetype = c("glob", "regex", "fixed"),
                           separator = " ",
                           case_insensitive = TRUE, ...) {
    dots <- list(...)
    x <- as.tokens(x)
    valuetype <- match.arg(valuetype)
    window <- check_integer(window, 1, 1, 0)
    separator <- check_character(separator)
    
    attrs <- attributes(x)
    type <- types(x)
    if (is.list(pattern) && is.null(names(pattern)))
        names(pattern) <- pattern
    ids <- pattern2list(pattern, type, valuetype,
                        case_insensitive, field_object(attrs, "concatenator"))
    result <- qatd_cpp_kwic(x, type, ids)
    result$pattern <- factor(result$pattern, levels = unique(names(ids)))
    if (nrow(result)) {
        r <- order(match(result$docname, docnames(x)),
                   result$from, result$to, result$pattern)
        result <- result[r,]
    }
    rownames(result) <- NULL
    attr(result, "tokens") <- x
    attr(result, "window") <- window
    attr(result, "separator") <- separator
    class(result) <- c("kwic", "data.frame")
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

#' @rdname kwic
#' @method as.data.frame kwic
#' @export
as.data.frame.kwic <- function(x, ..., window = NULL, separator = NULL) {
    
    if (is.null(window))
        window <- attr(x, "window")
    else
        window <- check_integer(window, 1, 1, 0)
    
    if (is.null(separator))
        separator <- attr(x, "separator")
    else
        separator <- check_character(separator)
    
    attrs <- attributes(x)
    x$pre <- rep("", nrow(x))
    x$keyword <- rep("", nrow(x))
    x$post <- rep("", nrow(x))
    if (nrow(x)) {
        toks <- attrs$tokens[x$docname]
        lis_pre <- as.list(tokens_select(toks, startpos = pmax(x$from - window, 1), endpos = x$to - 1))
        lis_key <- as.list(tokens_select(toks, startpos = x$from, endpos = x$to))
        lis_post <- as.list(tokens_select(toks, startpos = x$from + 1, endpos = x$to + window))
        
        x$pre[lengths(lis_pre) > 0] <- stri_c_list(lis_pre, sep = separator)
        x$keyword[lengths(lis_key) > 0] <- stri_c_list(lis_key, sep = separator)
        x$post[lengths(lis_post) > 0] <- stri_c_list(lis_post, sep = separator)
    }
    attr(x, "tokens") <- NULL
    attr(x, "window") <- NULL
    attr(x, "separator") <- NULL
    class(x) <- "data.frame"
    return(x)
}

#' @rdname kwic
#' @method print kwic
#' @param window the number of context words to be displayed around the keyword.
#' @param separator character to separate words in the output
#' @importFrom stringi stri_c stri_c_list
#' @export
print.kwic <- function(x, window = NULL, separator = NULL, ...) {
    
    attrs <- attributes(x)
    x <- as.data.frame(x, window = window, separator = separator)
    
    cat(sprintf("Keyword-in-context with %d match%s.", nrow(x), ifelse(nrow(x) != 1, "es", "")))
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
        invisible(result)
    }
}

#' @method [ kwic
#' @export
#' @noRd
`[.kwic` <- function(x, i, j, ...) {
    attrs <- attributes(x)
    class(x) <- c("data.frame")
    x <- x[i,]
    attr(x, "tokens") <- attrs$tokens
    class(x) <- c("kwic", "data.frame")
    return(x)
}
