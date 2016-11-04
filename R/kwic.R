#' List key words in context from a text or a corpus of texts.
#' 
#' For a text or a collection of texts (in a quanteda corpus object), return a 
#' list of a keyword supplied by the user in its immediate context, identifying 
#' the source text and the word index number within the source text.  (Not the 
#' line number, since the text may or may not be segmented using end-of-line 
#' delimiters.)
#' 
#' @param x a text character, quanteda corpus, or tokenizedTexts object
#' @param keywords A keyword pattern or phrase consisting of multiple keyword 
#'   patterns, possibly including punctuation.  If a phrase, \code{keywords} 
#'   will be tokenized using the \code{...} options.
#' @param window The number of context words to be displayed around the keyword.
#' @param valuetype how to interpret keyword expressions: \code{"glob"} for 
#'   "glob"-style wildcard expressions; \code{"regex"} for regular expressions; 
#'   or \code{"fixed"} for exact matching (entire words, for instance).  If 
#'   \code{"fixed"} is used with \code{case_insensitive = TRUE}, the text will 
#'   be lowercased prior to matching.
#' @param case_insensitive match without respect to case if \code{TRUE}
#' @param ... additional arguments passed to \link{tokenize}, for applicable 
#'   methods
#' @return A kwic object classed data.frame, with the document name 
#'   (\code{docname}), the token index position (\code{position}), the context
#'   before (\code{contextPre}), the keyword in its original format
#'   (\code{keyword}, preserving case and attached punctuation), and the context
#'   after (\code{contextPost}).
#' @author Kenneth Benoit
#' @export
#' @examples
#' head(kwic(inaugTexts, "secure*", window = 3, valuetype = "glob"))
#' head(kwic(inaugTexts, "secur", window = 3, valuetype = "regex"))
#' head(kwic(inaugTexts, "security", window = 3, valuetype = "fixed"))
#' 
#' kwic(inaugCorpus, "war against")
#' kwic(inaugCorpus, "war against", valuetype = "regex")
kwic <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    UseMethod("kwic")
}

#' @rdname kwic
#' @method kwic character
#' @export
kwic.character <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(tokenize(x, ...), keywords, window, valuetype, case_insensitive)
}

#' @rdname kwic
#' @method kwic corpus
#' @export 
kwic.corpus <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    kwic(texts(x), keywords, window, valuetype, case_insensitive, ...)
}

#' @rdname kwic
#' @method kwic tokenizedTexts
#' @export 
kwic.tokenizedTexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    valuetype <- match.arg(valuetype)
    keywordsTokenized <- tokenize(keywords, simplify = TRUE, what = "fastestword", ...)
    contexts <- lapply(x, kwic.tokenizedText, keywordsTokenized, window, valuetype, case_insensitive)
    if (sum(is.na(contexts)) == length(contexts)) return(NA) # means no search term found
    # name the text vector
    if (!is.null(names(x))) {
        names(contexts) <- names(x)
    } else {
        names(contexts) <- paste("text", 1:length(x), sep="")
    }
    contexts <- contexts[!is.na(contexts)]
    contexts <- cbind(docname = rep(names(contexts), sapply(contexts, nrow)), do.call(rbind, contexts))
    row.names(contexts) <- NULL
    contexts$contextPre <- stringi::stri_trim_right(contexts$contextPre)
    contexts$contextPre <- stringi::stri_replace_all_regex(contexts$contextPre, "(\\w*) (\\W)", "$1$2")
    contexts$contextPre <- format(contexts$contextPre, justify="right")
    contexts$contextPost <- stringi::stri_replace_all_regex(contexts$contextPost, "(\\w*) (\\W)", "$1$2")
    contexts$contextPost <- format(contexts$contextPost, justify="left")
    # contexts$keyword <- format(contexts$keyword, justify="left")
    contexts$keyword <- stringi::stri_trim_both(contexts$keyword)
    
    attr(contexts, "valuetype") <- valuetype

    
    #  If these tokenized texts are not named, then their ntokens will not have names either
    ntoken <- ntoken(x)
    if (is.null(names(ntoken)))
        names(ntoken) <- paste("text", 1:length(x), sep="")

    attr(contexts, "ntoken")  <- ntoken

    attr(contexts, "keywords") <- keywords
    attr(contexts, "tokenize_opts") <- list(...)
    class(contexts) <- c("kwic", class(contexts))
    contexts
}

kwic.tokenizedText <- function(x, word, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE) {
    valuetype <- match.arg(valuetype)
    
    wordLength <- length(word)
    
#     catm("valuetype = ", valuetype, "\n")
#     catm("case_insensitive = ", case_insensitive, "\n")
    
    if (valuetype == "fixed") {
        if (case_insensitive)
            startMatchIndex <- matchFixed(x, word, case_insensitive)
        else
            startMatchIndex <- match(x, word)
    } else if (valuetype == "glob") {
        startMatchIndex <- matchRegex(x, utils::glob2rx(word), case_insensitive)
    } else {
        startMatchIndex <- matchRegex(x, word, case_insensitive)
    }

    # vectorized location of keyword position
    endMatchIndex <- matchSequence(1:wordLength, startMatchIndex)
    if (!length(endMatchIndex)) return(NA)
    
    if (wordLength > 1) {
        indexKeyword <- cbind(endMatchIndex, endMatchIndex + wordLength - 1)
        position <- sapply(endMatchIndex, function(y) paste(c(y, y + wordLength - 1), collapse = ":"))
    } else {
        position <- endMatchIndex
        indexKeyword <- cbind(endMatchIndex, endMatchIndex)
    }


    indexPre <- cbind(pmax(endMatchIndex - window, 0), pmax(endMatchIndex - 1, 0))
    indexPost <- cbind(
       ifelse(endMatchIndex + wordLength < length(x), endMatchIndex + wordLength, NA),
       pmin(endMatchIndex + wordLength + window-1, length(x))
    )
    concatIndexes <- function(indexPair, textVector) {
        if (any(is.na(indexPair))) return("")
        if (identical(indexPair, c(0,0))) return("")
        if (length(indexPair) == 1)
            textVector[indexPair]
        else
            paste(textVector[indexPair[1]:indexPair[2]], collapse = " ")
    }
    
    result <- data.frame(position, 
                         contextPre = apply(indexPre, 1, concatIndexes, x),
                         keyword = apply(indexKeyword, 1, concatIndexes, x),
                         contextPost = apply(indexPost, 1, concatIndexes, x),
                         stringsAsFactors = FALSE)

    # override pre/post if window = 0
    if (window == 0) {
        result$contextPre <- result$contextPost <- ""
    }
    
    # left-justify the post-word part
    result$contextPre <- format(result$contextPre, justify="left")
    # trim trailing spaces from keyword
    result$keyword <- stringi::stri_trim_both(result$keyword)
    class(result) <- c("kwic", class(result))
    result
}

#' @rdname kwic
#' @method print kwic
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




## solution from alexis_laz 
## http://stackoverflow.com/questions/33027611/how-to-index-a-vector-sequence-within-a-vector-sequence
matchSequence = function(pat, x) {
    ff = function(.pat, .x, acc = if (length(.pat)) seq_along(.x) else integer(0)) {
        if (!length(.pat)) return(acc)
        Recall(.pat[-1], .x, acc[which(.pat[1] == .x[acc])] + 1)
    }
    ff(pat, x) - length(pat)
}

# helper function for matchSequence
# wraps a vector by removing the first n elements and padding end with NAs
wrapVector <- function(x, n) {
    stopifnot(n <= length(x))
    if (n == length(x)) 
        return(rep(NA, n))
    else
        return(c(x[(n+1):length(x)], rep(NA, n)))
}


# works like match but for regular expressions
matchRegex <- function(x, table, case_insensitive) {
    result <- rep(NA, length(x))
    for (i in 1:length(table)) {
        # logMatch <- grepl(table[i], x)
        logMatch <- !is.na(stringi::stri_locate_first_regex(x, table[i], opts_regex = list(case_insensitive = case_insensitive))[, 1])
        result[which(logMatch)] <- i
    }
    result
}

# works like match but for regular expressions
matchFixed <- function(x, table, case_insensitive) {
    table <- paste0("^", table, "$")
    matchRegex(x, table, case_insensitive)
    
#     result <- rep(NA, length(x))
#     for (i in 1:length(table)) {
#         # logMatch <- grepl(table[i], x)
#         logMatch <- !is.na(stringi::stri_locate_first_fixed(x, table[i], opts_fixed = list(case_insensitive = case_insensitive))[, 1])
#         result[which(logMatch)] <- i
#     }
#     result
}
