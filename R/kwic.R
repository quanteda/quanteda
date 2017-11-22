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
    kwic(corpus(x), pattern, window, valuetype, case_insensitive, join, ...)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.corpus <- function(x, pattern, window = 5, valuetype = c("glob", "regex", "fixed"), 
                        case_insensitive = TRUE, join = FALSE, ...) {
    #thecall <- as.list(match.call())[-1]
    # if ("keywords" %in% names(thecall)) {
    #     .Deprecated(msg = "keywords argument has been replaced by pattern")
    #     names(thecall)[which(names(thecall) == "keywords")] <- "pattern"
    #     thecall[["x"]] <- x
    #     print(thecall)
    #     return(do.call(kwic, thecall))
    # }    
    if (is.collocations(pattern) || is.dictionary(pattern))
        pattern <- phrase(pattern) 
    kwic(tokens(x, ...), pattern, window, valuetype, case_insensitive, join)
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
        names(x) <- paste0(quanteda_options("base_docname"), seq_len(x))
    }
    
    keywords_id <- pattern2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    temp <- qatd_cpp_kwic(x, types, keywords_id, window, join)
    
    # attributes for kwic object
    result <- structure(temp, 
                        class = c("kwic", "data.frame"), 
                        ntoken = ntoken(x), 
                        valuetype = valuetype, 
                        keywords = attr(keywords_id, 'pattern'),
                        tokens =  attr(temp, "tokens"))
    attributes(result, FALSE)  <- attributes(x)
    return(result)
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
#' @return \code{as.tokens.kwic} converts the kwic object into a \link{tokens}
#'   object, with each new "document" consisting of one keyword match, and the
#'   contents of the \code{pre}, \code{keyword}, and \code{post} fields forming
#'   the tokens.  This is one way to save the output for subsequent usage;
#'   another way is to form a \link[=corpus.kwic]{corpus} from the return
#'   object.
as.tokens.kwic <- function(x, ...) {
    vars <- docvars(x)
    vars[['_docid']] <- attr(x, 'docid')
    vars[['_segid']] <- attr(x, 'segid')
    result <- structure(attr(x, 'tokens'), 
                        class = 'tokens',
                        names = rownames(vars),
                        docvars = vars)
    attributes(result, FALSE) <- attributes(x)
    return(result)
}


