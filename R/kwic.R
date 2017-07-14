#' locate keywords-in-context
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
#' @param join join adjacent keywords in the concordance view if \code{TRUE}
#' @param ... additional arguments passed to \link{tokens}, for applicable 
#'   object types
#' @return A kwic object classed data.frame, with the document name 
#'   (\code{docname}), the token index position (\code{position}), the context 
#'   before (\code{contextPre}), the keyword in its original format 
#'   (\code{keyword}, preserving case and attached punctuation), and the context
#'   after (\code{contextPost}).
#' @note \code{pattern} will be a keyword pattern or phrase, possibly multiple
#'   patterns, that may include punctuation.  If a pattern contains whitespace,
#'   it is best to wrap it in \code{\link{phrase}} to make this explicit. 
#'   However if \code{pattern} is a \link[=textstat_collocations]{collocations}
#'   or \link{dictionary} object, then the collocations or multi-word dictionary
#'   keys will automatically be considered phrases where each
#'   whitespace-separated element matches a token in sequence.
#' @author Kenneth Benoit and Kohei Watanabe
#' @export
#' @examples
#' head(kwic(data_corpus_inaugural, "secure*", window = 3, valuetype = "glob"))
#' head(kwic(data_corpus_inaugural, "secur", window = 3, valuetype = "regex"))
#' head(kwic(data_corpus_inaugural, "security", window = 3, valuetype = "fixed"))
#' 
#' toks <- tokens(data_corpus_inaugural)
#' kwic(data_corpus_inaugural, phrase("war against"))
#' kwic(data_corpus_inaugural, phrase("war against"), valuetype = "regex")
#' 

kwic <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                 case_insensitive = TRUE, join = FALSE, ...) {
    UseMethod("kwic")
}

#' @rdname kwic
#' @noRd
#' @export
kwic.character <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                           case_insensitive = TRUE, join = FALSE, ...) {
    thecall <- as.list(match.call())[-1]
    if ("keywords" %in% names(thecall)) {
        .Deprecated(msg = "keywords argument has been replaced by pattern")
        names(thecall)[which(names(thecall) == "keywords")] <- "pattern"
        return(do.call(kwic, thecall))
    }    
    if (is.collocations(pattern) || is.dictionary(pattern))
        pattern <- phrase(pattern) 
    kwic(tokens(x, ...), pattern, window, valuetype, case_insensitive, join)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.corpus <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                        case_insensitive = TRUE, join = FALSE, ...) {
    kwic(texts(x), pattern, window, valuetype, case_insensitive, join, ...)
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
#' @export 
kwic.tokens <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                        case_insensitive = TRUE, join = FALSE, ...) {
    
    if ("keywords" %in% names(arglist <- list(...))) {
        .Deprecated(msg = "keywords argument has been replaced by pattern")
        return(kwic(x, pattern = arglist$keywords, window, valuetype, case_insensitive, join))
    }    

    valuetype <- match.arg(valuetype)
    types <- types(x)
    
    # add document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    keywords_id <- features2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    result <- qatd_cpp_kwic(x, types, keywords_id, window, join)
    
    # attributes for tokens object
    attributes(attr(result, "tokens"), FALSE)  <- attributes(x)
    
    # attributes for kwic object
    attr(result, "ntoken") <- ntoken(x)
    attr(result, "valuetype") <- valuetype
    attr(result, "keywords") <- attr(keywords_id, 'features')
    attributes(result, FALSE)  <- attributes(x)
    class(result) <- c("kwic", "data.frame")
    return(result)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.tokenizedTexts <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                                case_insensitive = TRUE, join = FALSE, ...) {
    kwic(as.tokens(x), pattern, window, valuetype, case_insensitive, join, ...)
}

#' @rdname kwic
#' @export
#' @examples
#' mykwic <- kwic(data_corpus_inaugural, "provident*")
#' is.kwic(mykwic)
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
            labels <- stri_c("[", x$docname, ", ", x$from, ':', x$to, "]")
        }
        kwic <- data.frame(
            label = labels,
            pre = format(stri_replace_all_regex(x$pre, "(\\w*) (\\W)", "$1$2"), justify="right"),
            s1 = rep('|', nrow(x)),
            keyword = format(x$keyword, justify="centre"),
            s2 = rep('|', nrow(x)),
            post = format(stri_replace_all_regex(x$post, "(\\w*) (\\W)", "$1$2"), justify="left")
        )
        colnames(kwic) <- NULL
        print(kwic, row.names = FALSE)
    }
}

#' @rdname kwic
#' @export
#' @method as.tokens kwic
as.tokens.kwic <- function(x, ...) {
    result <- attr(x, 'tokens')
    names(result) <- x$docname
    docvars(result) <- data.frame('_docid' = attr(x, 'docid'),
                                  '_segid' = attr(x, 'segid'))
    return(result)
}
