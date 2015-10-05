#' List key words in context from a text or a corpus of texts.
#' 
#' For a text or a collection of texts (in a quanteda corpus object), return a 
#' list of a keyword supplied by the user in its immediate context, identifying 
#' the source text and the word index number within the source text.  (Not the 
#' line number, since the text may or may not be segmented using end-of-line 
#' delimiters.)
#' 
#' @param x A text character scalar or a quanteda corpus.  (Currently does not 
#'   support character vectors.)
#' @param keywords A keyword or phrase consisting of multiple keywords, possibly
#'   including punctuation.  If a phrase, \code{keywords} will be tokenized 
#'   using the \code{...} options.
#' @param window The number of context words to be displayed around the keyword.
#' @param valuetype how to interpret keyword expressions: \code{"glob"} for 
#'   "glob"-style wildcard expressions; \code{"regex"} for regular expressions;
#'   or \code{"fixed"} for exact matching (entire words, for instance).  If
#'   \code{"fixed"} is used with \code{case_insensitive = TRUE}, the text will
#'   be lowercased prior to matching.
#' @param case_insensitive match without respect to case if \code{TRUE}
#' @param ... additional arguments passed to \link{tokenize}, for applicable methods
#' @return A data frame with the context before (\code{preword}), the keyword in
#'   its original format (\code{word}, preserving case and attached 
#'   punctuation), and the context after (\code{postword}).  The rows of the 
#'   dataframe will be named with the word index position, or the text name and 
#'   the index position for a corpus object.
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
    keywords <- tokenize(keywords, simplify = TRUE, what = "fastestword", ...)
    contexts <- contexts <- lapply(x, kwic.tokenizedText, keywords, window, valuetype, case_insensitive)
    if (sum(is.na(contexts)) == length(contexts)) return(NA) # means no search term found
    # name the text vector
    if (!is.null(names(x))) {
        names(contexts) <- names(x)
    } else {
        names(contexts) <- paste("text", 1:length(x), sep="")
    }
    contexts <- contexts[!is.na(contexts)]
    contexts <- do.call("rbind", contexts)
    contexts$position <- paste0("[", gsub("^(.*)\\.\\d+$", "\\1", rownames(contexts)), ", ", contexts$position, "]")
    contexts$position <- format(contexts$position, justify="right")
    contexts$contextPre <- stringi::stri_trim_right(contexts$contextPre)
    contexts$contextPre <- stringi::stri_replace_all_regex(contexts$contextPre, "(\\w*) (\\W)", "$1$2")
    contexts$contextPre <- format(contexts$contextPre, justify="right")
    contexts$contextPost <- stringi::stri_replace_all_regex(contexts$contextPost, "(\\w*) (\\W)", "$1$2")
    contexts$contextPost <- format(contexts$contextPost, justify="left")
    contexts$keyword <- format(contexts$keyword, justify="centre")
    rownames(contexts) <- contexts$position
    contexts$position <- NULL
    #names(contexts)[c(1,3)] <- c("pre", "post")
    contexts
}

kwic.tokenizedText <- function(x, word, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE) {
    valuetype <- match.arg(valuetype)
    
    wordLength <- length(word)
    
#     cat("valuetype = ", valuetype, "\n")
#     cat("case_insensitive = ", case_insensitive, "\n")
    
    if (valuetype == "fixed") {
        if (case_insensitive)
            matchIndex <- matchFixed(x, word, case_insensitive)
        else
            matchIndex <- match(x, word)
    } else if (valuetype == "glob") {
        matchIndex <- matchRegex(x, utils::glob2rx(word), case_insensitive)
    } else {
        matchIndex <- matchRegex(x, word, case_insensitive)
    }

    # vectorized location of keyword position
    matchIndex2 <- matchSequence(1:wordLength, matchIndex)
    if (!length(matchIndex2)) return(NA)
    
    if (wordLength > 1) {
        indexKeyword <- cbind(matchIndex2, matchIndex2 + wordLength - 1)
        position <- sapply(matchIndex2, function(y) paste(c(y, y + wordLength - 1), collapse = ":"))
    } else {
        position <- matchIndex2
        indexKeyword <- cbind(matchIndex2, matchIndex2)
    }

    indexPre <- cbind(pmax(matchIndex2 - window, 1), pmax(matchIndex2 - 1, 1))
    indexPost <- cbind(pmin(matchIndex2 + wordLength, length(x)), pmin(matchIndex2 + wordLength + window-1, length(x)))
    
    concatIndexes <- function(indexPair, textVector) {
        if (length(indexPair) == 1)
            textVector[indexPair]
        else
            paste(textVector[indexPair[1]:indexPair[2]], collapse = " ")
    }
    
    result <- data.frame(position, 
                         contextPre = apply(indexPre, 1, concatIndexes, x),
                         keyword = apply(indexKeyword, 1, concatIndexes, x),
                         contextPost = apply(indexPost, 1, concatIndexes, x))
    
    # left-justify the post-word part
    result$contextPre <- format(result$contextPre, justify="left")
    # centre the post-word part
    result$keyword <- format(result$keyword, justify="centre")
    class(result) <- c("kwic", class(result))
    result
}


# return the first index position of a sequence seq in a vector of values vec
matchSequence <- function(seq, vec) {
    vecTrimmed <- vec[1 : (length(vec) - length(seq) + 1)]
    matches <- seq[1] == vecTrimmed
    if (length(seq) == 1) return(which(matches))
    for (i in 2:length(seq)) {
        matches <- cbind(matches, seq[i] == wrapVector(vecTrimmed, i - 1))
    }
    which(rowSums(matches) == i)
}

wrapVector <- function(x, n, window = 1) {
    if (n == length(x)) 
        return(x)
    while (n > length(x)) 
        n <- n - 20
    result <- c(x[(n+1):length(x)], x[1:n])
    if (window > 1)
        result[(length(result) - n + 1) : length(result)] <- NA
    result
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


kwicSingleText <- function(text, word, window = 5, wholeword=FALSE) {
    # don't use tokenize since we want to preserve case and punctuation here
    # grep needed to get words that end in punctuation mark or in quotes
    tokens <- strsplit(text, " ")[[1]]
    # interpret word as a regular expression if regex==TRUE
    match.expression <- ifelse(wholeword, 
                               paste("^", tolower(word), "$", sep=""),
                               tolower(word))
    matches <- grep(match.expression, tolower(tokens))
    if (length(matches) == 0) return(NA)
    result <- data.frame(source = matches, 
                         preword = NA,
                         word = tokens[matches],
                         postword = NA)
    for (m in 1:length(matches)) {
        wordpos  <- matches[m]
        startpos <- ifelse(((matches[m] - window) < 1), 1, matches[m] - window)
        endpos   <- ifelse(((matches[m] + window) > length(tokens)), length(tokens), matches[m] + window)
        result$preword[m]  <-  ifelse(wordpos==startpos, "", paste(tokens[startpos : (wordpos - 1)], collapse = " "))
        result$postword[m] <-  ifelse(wordpos==endpos, "", paste(tokens[(wordpos + 1) : endpos], collapse = " "))
    }
    # left-justify the post-word part
    result$postword <- format(result$postword, justify="left")
    # centre the post-word part
    result$word <- format(result$word, justify="centre")
    class(result) <- c("kwic", class(result))
    return(result)
}


