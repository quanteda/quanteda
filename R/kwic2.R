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
#' head(kwic(data_char_inaugural, "secure*", window = 3, valuetype = "glob", new = TRUE))
#' head(kwic(data_char_inaugural, "secur", window = 3, valuetype = "regex", new = TRUE))
#' head(kwic(data_char_inaugural, "security", window = 3, valuetype = "fixed", new = TRUE))
#' 
#' kwic(data_corpus_inaugural, "war against", new = TRUE)
#' kwic(data_corpus_inaugural, "war against", valuetype = "regex", new = TRUE)
#' 
kwic <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = FALSE) {
    if (!new) 
        UseMethod("kwic_old")
    else
        UseMethod("kwic")
}

#' @rdname kwic
#' @noRd
#' @export
kwic.character <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = FALSE) {
    kwic(tokens(x, ...), keywords, window, valuetype, case_insensitive, new = TRUE)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.corpus <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = FALSE) {
    kwic(texts(x), keywords, window, valuetype, case_insensitive, ..., new = TRUE)
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
#' kwic(txt, c("is", "a"), valuetype = "fixed", new = TRUE)
#' kwic(txt, list("is", "a", c("is", "it")), valuetype = "fixed", new = TRUE)
#' 
#' toks <- tokens(txt)
#' kwic(toks, c("is", "a"), valuetype = "fixed", new = TRUE)
#' kwic(toks, list("is", "a", c("is", "it")), valuetype = "fixed", new = TRUE)
#' @export 
kwic.tokens <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ..., new = FALSE) {
    
    valuetype <- match.arg(valuetype)
    keywords <- vector2list(keywords)
    
    # add document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    types <- types(x)
    keywords_fixed <- regex2fixed5(keywords, types, valuetype, case_insensitive) # convert glob or regex to fixed
    keywords_id <- lapply(keywords_fixed, function(x) fastmatch::fmatch(x, types))

    detect <- qatd_cpp_tokens_detect(x, keywords_id)
    ids <- which(sapply(detect, sum, USE.NAMES = FALSE) > 0)
    if (length(ids) == 0) return(NULL) # nothing is found
    
    # build up result
    df_result <- data.frame()
    for (id in ids) {
        df_temp <- kwic_split(x[[id]], detect[[id]], window)
        df_temp$docname <- rep(names(x)[id], nrow(df_temp))
        df_result <- rbind(df_result, df_temp, stringsAsFactors=FALSE)
    }

    class(df_result) <- c("kwic2", "kwic", class(df_result))
    df_result
}

#' @rdname kwic
#' @noRd
#' @export 
kwic.tokenizedTexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(as.tokens(x), keywords, window, valuetype, case_insensitive, ..., new = TRUE)
}


#' split kwic results
#'
#' Helper function for kwic designed to parse sequence matches into contexts and keywords.
#' @param char character vector
#' @param mask numeric vector with the same length as char where 1 indicate appearance of keywords
#' @param window size of the pre and post contexts
#' @author Kohei Watanabe
#' @examples
#' \dontrun{
#' kwic_split(letters[1:5], c(0, 1, 1, 0, 1), window = 1)
#' kwic_split(letters[1:5], c(1, 1, 1, 0, 1), window = 1)
#' }
#' @keywords internal
kwic_split <- function(char, mask, window) {
    
    # Expand vector for matches at the top or end
    char <- c('', char, '')
    mask <- c(0, mask, 0)
    len <- length(char)
    
    # Detect starting and ending positions of keywords
    start <- which(diff(c(0, mask)) == 1)
    end <- which(diff(c(mask, 0)) == -1)
    
    pre <- target <- post <- c()
    for (i in 1:length(start)) {
        pre <- c(pre, stringi::stri_c(char[max(0, start[i] - window):max(0, start[i] - 1)], collapse = ' '))
        target <- c(target, stringi::stri_c(char[start[i]:end[i]], collapse = ' '))
        post <- c(post, stringi::stri_c(char[min(len + 1, end[i] + 1):min(len, end[i] + window)], collapse = ' '))
    }
    return(data.frame(position = stringi::stri_c(start - 1, end  - 1, sep = ':'), 
                      contextPre = pre, keyword = target, contextPost = post, stringsAsFactors = FALSE))
}

#' @method print kwic
#' @noRd
#' @export
print.kwic <- function(x, ...) {
    contexts <- x
    contexts$positionLabel <- paste0("[", contexts$docname, ", ", contexts$position, "]")
    contexts$positionLabel <- format(contexts$positionLabel, justify="right")
    contexts$keyword <- format(contexts$keyword, justify="centre")
    rownames(contexts) <- contexts$positionLabel
    contexts$positionLabel <- contexts$docname <- contexts$position <- NULL
    contexts$contextPre <- paste(contexts$contextPre, "[")
    contexts$contextPost <- paste("]", contexts$contextPost)
    print(as.data.frame(contexts))
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

#' @rdname kwic
#' @details \code{as.kwic} is a temporary function to convert a "kwic2" to a standard 
#' "kwic" object.
#' @export
#' @examples 
#' # as.kwic examples
#' txt <- c("This is a test",
#'          "This is it.",
#'          "What is in a train?",
#'          "Is it a question?",
#'          "Sometimes you don't know if this is it.",
#'          "Is it a bird or a plane or is it a train?")
#' 
#' toks <- tokens(txt)
#' (kwOld <- kwic(toks, "is it", new = FALSE))
#' (kwNew <- kwic(toks, "is it", new = TRUE))
#' \dontrun{
#' # this breaks - need to harmonize print methods
#' as.kwic(kwNew)
#' }
as.kwic <- function(x) {
    # strip "kwic2" from class list
    if (class(x)[1] == "kwic2" & is.kwic(x))
        class(x) <- class(x)[-1]
    x
}


#' @method print kwic2
#' @noRd
#' @export
print.kwic2 <- function(x, ...) {

    # x$before <- format(x$before, justify="right")
    # x$keyword <- stri_c('[', x$keyword, ']', sep = '')
    # x$after <- format(x$after, justify="left")
    # print(as.data.frame(x))

    df <- data.frame(
        before = format(x$contextPre, justify="right"),
        s1 = rep('|', nrow(x)),
        keyword = format(x$keyword, justify="centre"),
        s2 = rep('|', nrow(x)),
        after = format(x$contextPost, justify="left")
    )
    #colnames(df) <- c('Before', '', '', '', 'After')
    colnames(df) <- NULL
    rownames(df) <- stringi::stri_c("[", x$docname, " ", x$position, "]")
    print(df)
}

