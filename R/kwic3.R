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
#' @param new logical; if \code{TRUE} use the newer \code{kwic}, if \code{FALSE}
#' then call \code{\link{kwic_old}}.  Once the full
#' testing of the newer \link{kwic} method is complete and the transition
#' declared successful, we will delete this option and delete \code{kwic_old}.
#' @return A kwic object classed data.frame, with the document name 
#'   (\code{docname}), the token index position (\code{position}), the context
#'   before (\code{contextPre}), the keyword in its original format
#'   (\code{keyword}, preserving case and attached punctuation), and the context
#'   after (\code{contextPost}).
#' @author Kenneth Benoit and Kohei Watanabe
#' @export
#' @examples
#' head(kwic(data_char_inaugural, "secure*", window = 3, valuetype = "glob"))
#' head(kwic(data_char_inaugural, "secur", window = 3, valuetype = "regex"))
#' head(kwic(data_char_inaugural, "security", window = 3, valuetype = "fixed"))
#' \dontrun{
#' toks <- tokens(data_char_inaugural)
#' microbenchmark::microbenchmark(
#'    kwic(toks, "the", window = 3, valuetype = "fixed"),
#'    kwic_cpp(toks, "the", window = 3, valuetype = "fixed"))
#' }
#' kwic(data_corpus_inaugural, "war against")
#' kwic(data_corpus_inaugural, "war against", valuetype = "regex")
#' 
kwic_cpp <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = TRUE) {
        UseMethod("kwic_cpp")
}

#' @rdname kwic
#' @noRd
#' @export
kwic_cpp.character <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = TRUE) {
    kwic_cpp(tokens(x, ...), keywords, window, valuetype, case_insensitive, new = TRUE)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic_cpp.corpus <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = TRUE) {
    kwic_cpp(texts(x), keywords, window, valuetype, case_insensitive, ..., new = TRUE)
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
kwic_cpp.tokens <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = TRUE) {
    
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    valuetype <- match.arg(valuetype)
    keywords <- vector2list(keywords)

    # add document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    types <- types(x)
    keywords_id <- regex2id(keywords, types, valuetype, case_insensitive, FALSE)
    result <- qatd_cpp_kwic(x, types, keywords_id, window)
    result$document <- as.factor(result$document)
    
    if (!nrow(result)) 
        return(NULL)
    
    # add attributes for kwic object
    attr(result, "valuetype") <- valuetype
    attr(result, "keywords") <- sapply(keywords, paste, collapse = " ")
    class(result) <- c("kwic_cpp", "data.frame")
    return(result)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic_cpp.tokenizedTexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = TRUE) {
    kwic_cpp(as.tokens(x), keywords, window, valuetype, case_insensitive, ..., new = TRUE)
}


#' @rdname kwic
#' @export
#' @examples
#' mykwic <- kwic(data_corpus_inaugural, "provident*")
#' is.kwic(mykwic)
#' is.kwic("Not a kwic")
is.kwic_cpp <- function(x) {
    ifelse("kwic_cpp" %in% class(x), TRUE, FALSE)
}

#' @method print kwic_cpp
#' @noRd
#' @export
print.kwic_cpp <- function(x, ...) {
    if (!length(x)) {
        print(NULL)
        return()
    }
    kwic <- data.frame(
        pre = format(stringi::stri_replace_all_regex(x$pre, "(\\w*) (\\W)", "$1$2"), justify="right"),
        s1 = rep('|', nrow(x)),
        keyword = format(x$target, justify="centre"),
        s2 = rep('|', nrow(x)),
        post = format(stringi::stri_replace_all_regex(x$post, "(\\w*) (\\W)", "$1$2"), justify="left")
    )
    colnames(kwic) <- NULL
    rownames(kwic) <- stringi::stri_c("[", x$document, ", ", x$position, "]")
    print(kwic)
}
