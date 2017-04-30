#' locate keywords-in-context
#' 
#' For a text or a collection of texts (in a quanteda corpus object), return a 
#' list of a keyword supplied by the user in its immediate context, identifying 
#' the source text and the word index number within the source text.  (Not the 
#' line number, since the text may or may not be segmented using end-of-line 
#' delimiters.)
#' @param x a character, \link{corpus}, or \link{tokens} object
#' @param keywords a keyword pattern or phrase consisting of multiple keyword 
#'   patterns, possibly including punctuation.  If a phrase, \code{keywords} 
#'   will be tokenized using the \code{...} options.
#' @param window the number of context words to be displayed around the keyword.
#' @inheritParams valuetype
#' @param case_insensitive match without respect to case if \code{TRUE}
#' @param ... additional arguments passed to \link{tokens}, for applicable 
#'   object types
#' @return A kwic object classed data.frame, with the document name 
#'   (\code{docname}), the token index position (\code{position}), the context
#'   before (\code{contextPre}), the keyword in its original format
#'   (\code{keyword}, preserving case and attached punctuation), and the context
#'   after (\code{contextPost}).
#' @author Kenneth Benoit and Kohei Watanabe
#' @export
#' @examples
#' head(kwic(data_corpus_inaugural, "secure*", window = 3, valuetype = "glob"))
#' head(kwic(data_corpus_inaugural, "secur", window = 3, valuetype = "regex"))
#' head(kwic(data_corpus_inaugural, "security", window = 3, valuetype = "fixed"))
#' 
#' toks <- tokens(data_corpus_inaugural)
#' kwic(data_corpus_inaugural, "war against")
#' kwic(data_corpus_inaugural, "war against", valuetype = "regex")
#' 
kwic <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    UseMethod("kwic")
}

#' @rdname kwic
#' @noRd
#' @export
kwic.character <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(tokens(x, ...), keywords, window, valuetype, case_insensitive)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.corpus <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(texts(x), keywords, window, valuetype, case_insensitive, ...)
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
#' kwic(txt, list("is", "a", c("is", "it")), valuetype = "fixed")
#' 
#' toks <- tokens(txt)
#' kwic(toks, c("is", "a"), valuetype = "fixed")
#' kwic(toks, list("is", "a", c("is", "it")), valuetype = "fixed")
#' @export 
kwic.tokens <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    valuetype <- match.arg(valuetype)
    keywords <- features2list(keywords)

    # add document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    types <- types(x)
    keywords_id <- regex2id(keywords, types, valuetype, case_insensitive, FALSE)
    result <- qatd_cpp_kwic(x, types, keywords_id, window)
    #result$docname <- as.factor(result$docname)
    
    # attributes for tokens object
    attributes(attr(result, "tokens"), FALSE)  <- attributes(x)
    
    # attributes for kwic object
    attr(result, "ntoken") <- ntoken(x)
    attr(result, "valuetype") <- valuetype
    attr(result, "keywords") <- sapply(keywords, paste, collapse = " ")
    attributes(result, FALSE)  <- attributes(x)
    class(result) <- c("kwic", "data.frame")
    return(result)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.tokenizedTexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(as.tokens(x), keywords, window, valuetype, case_insensitive, ...)
}

#' @rdname kwic
#' @export
#' @examples
#' mykwic <- kwic(data_corpus_inaugural, "provident*")
#' is.kwic(mykwic)
#' is.kwic("Not a kwic")
is.kwic <- function(x) {
    ifelse("kwic" %in% class(x), TRUE, FALSE)
}

#' @method print kwic
#' @noRd
#' @export
print.kwic <- function(x, ...) {
    if (!nrow(x)) {
        print(NULL)
    } else {
        if (all(x$from == x$to)) {
            labels <- stringi::stri_c("[", x$docname, ", ", x$from, "]")
        } else {
            labels <- stringi::stri_c("[", x$docname, ", ", x$from, ':', x$to, "]")
        }
        kwic <- data.frame(
            label = labels,
            pre = format(stringi::stri_replace_all_regex(x$pre, "(\\w*) (\\W)", "$1$2"), justify="right"),
            s1 = rep('|', nrow(x)),
            keyword = format(x$keyword, justify="centre"),
            s2 = rep('|', nrow(x)),
            post = format(stringi::stri_replace_all_regex(x$post, "(\\w*) (\\W)", "$1$2"), justify="left")
        )
        colnames(kwic) <- NULL
        print(kwic, row.names = FALSE)
    }
}

#' @rdname kwic
#' @export
#' @method as.tokens kwic
as.tokens.kwic <- function(x) {
    result <- attr(x, 'tokens')
    names(result) <- x$docname
    docvars(result) <- data.frame('_docid' = attr(x, 'docid'),
                                  '_segid' = attr(x, 'segid'))
    return(result)
}
