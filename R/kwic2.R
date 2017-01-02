#' locate keywords-in-context
#' 
#' For a text or a collection of texts (in a quanteda corpus object), return a 
#' list of a keyword supplied by the user in its immediate context, identifying 
#' the source text and the word index number within the source text.  (Not the 
#' line number, since the text may or may not be segmented using end-of-line 
#' delimiters.)
#' 
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
#' @author Kenneth Benoit
#' @export
#' @examples
#' head(kwic(data_char_inaugural, "secure*", window = 3, valuetype = "glob"))
#' head(kwic(data_char_inaugural, "secur", window = 3, valuetype = "regex"))
#' head(kwic(data_char_inaugural, "security", window = 3, valuetype = "fixed"))
#' 
#' kwic(data_corpus_inaugural, "war against")
#' kwic(data_corpus_inaugural, "war against", valuetype = "regex")
#' 
kwic2 <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    UseMethod("kwic2")
}

#' @rdname kwic
#' @noRd
#' @export
kwic2.character <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic2(tokens(x, ...), keywords, window, valuetype, case_insensitive)
}

#' @rdname kwic
#' @noRd
#' @export 
kwic2.corpus <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic2(texts(x), keywords, window, valuetype, case_insensitive, ...)
}

#' @rdname kwic
#' @noRd
#' @examples 
#' mycorpus <- corpus(c("This is a test",
#' "This is it.",
#' "What is in a train?",
#' "Is it a question?",
#' "Sometimes you don't know if this is it.",
#' "Is it a bird or a plane or is it a train?"))
#' 
#' kwic2(mycorpus, c("is", "a"), valuetype = "fixed")
#' kwic2(mycorpus, list("is", "a", c("is", "it")), valuetype = "fixed")
#' @export 
kwic2.tokens <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    
    if (!is.tokens(x)) stop("x must be a tokens class object")
    
    keywords <- vector2list(keywords)
    valuetype <- match.arg(valuetype)
    
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    # Generate all combinations of type IDs
    entries_id <- list()
    types <- types(x)
    index <- index_regex(types, valuetype, case_insensitive) # index types before the loop
    
    #if (verbose) 
    #    message('Registering ', length(keywords), ' keywords...');
    for (h in 1:length(keywords)) {
        entries <- keywords[h]
        entries_fixed <- regex2fixed5(entries, types, valuetype, case_insensitive, index) # convert glob or regex to fixed
        if (length(entries_fixed) == 0) next
        entries_id <- c(entries_id, lapply(entries_fixed, function(x) fmatch(x, types)))
    }
    #if (verbose) 
    #    message('Searching ', length(entries_id), ' types of features...')
    
    # Detect keywords
    detect <- qatd_cpp_tokens_detect(x, entries_id)
    index <- which(sapply(detect, sum, USE.NAMES=FALSE) > 0)
    if(length(index) == 0) return(NULL) # nothing is found
    for(i in index){
        df_temp <- kwic_split(x[[i]], detect[[i]], window)
        rownames(df_temp) <- stri_c(names_org[i], rownames(df_temp), sep=':')
        if(i == 1){
            df <- df_temp
        }else{
            df <- rbind(df, df_temp, stringsAsFactors=FALSE)
        }
    }

    class(df) <- c("kwic2", class(df))
    return(df)
}

#kwic_split(letters[1:5], c(0, 1, 1, 0, 1), window=1)
#kwic_split(letters[1:5], c(1, 1, 1, 0, 1), window=1)

kwic_split <- function(char, mask, window){
    
    # Expand vector for matches at the top or end
    char <- c('', char, '')
    mask <- c(0, mask, 0)
    len <- length(char)
    start <- which(diff(c(0, mask))==1)
    end <- which(diff(c(mask, 0))==-1)
    
    pre <- target <- post <- c()
    for(i in 1:length(start)){
        pre <- c(pre, stri_c(char[max(0, start[i] - window):max(0, start[i] - 1)], collapse = ' '))
        target <- c(target, stri_c(char[start[i]:end[i]], collapse = ' '))
        post <- c(post, stri_c(char[min(len + 1, end[i] + 1):min(len, end[i] + window)], collapse = ' '))
    }
    #pre <- char[pmax(0, start - window):pmax(0, start - 1)]
    #target <- char[start:end]
    #post <- char[pmin(len, end + 1):pmin(len, end + window)]
    return(data.frame(before = pre, keyword = target, after = post, stringsAsFactors = FALSE))
    

    
}

#' @rdname kwic
#' @noRd
#' @export 
kwic2.tokenizedTexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic2(as.tokens(x), keywords, window, valuetype, case_insensitive, ...)
}

#' @method print kwic
#' @noRd
#' @export
print.kwic2 <- function(x, ...) {
    
    # x$before <- format(x$before, justify="right")
    # x$keyword <- stri_c('[', x$keyword, ']', sep = '')
    # x$after <- format(x$after, justify="left")
    # print(as.data.frame(x))
    
    df <- data.frame(
        before = format(x$before, justify="right"),
        s1 = rep('|', nrow(x)),
        keyword = format(x$keyword, justify="centre"),
        s2 = rep('|', nrow(x)),
        after = format(x$after, justify="left")
    )
    #colnames(df) <- c('Before', '', '', '', 'After')
    colnames(df) <- NULL
    rownames(df) <- rownames(x)
    print(df)
}

