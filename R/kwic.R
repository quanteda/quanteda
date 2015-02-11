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
#' @param word A keyword chosen by the user.
#' @param window The number of context words to be displayed around the 
#'   keyword.
#' @param wholeword If \code{TRUE}, then only search for the entire "word". 
#'   Otherwise \code{word} is interpreted as a regular expression, which
#'   matches any occurrence of \code{word} in the text, so that the the
#'   concordance will include all words in which the search term appears, and
#'   not just when it appears as an entire word.  For instance, searching for
#'   the word "key" will also return "whiskey".  This is the default.
#' @return A data frame with the context before (\code{preword}), the keyword 
#'   in its original format (\code{word}, preserving case and attached 
#'   punctuation), and the context after (\code{postword}).  The rows of the 
#'   dataframe will be named with the word index position, or the text name and
#'   the index position for a corpus object.
#' @author Kenneth Benoit and Paul Nulty
#' @export
#' @examples
#' kwic(inaugTexts, "terror")
#' kwic(inaugTexts, "terror", wholeword=TRUE)  # returns only whole word, without trailing punctuation
kwic <- function(x, word, window=5, wholeword=FALSE) {
    UseMethod("kwic")
}

kwicSingleText <- function(text, word, window=5, wholeword=FALSE) {
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


#' @rdname kwic
#' @method kwic character
#' @export
kwic.character <- function(x, word, window=5, wholeword=FALSE) {
    contexts <- lapply(x, kwicSingleText, word=word, window=window, wholeword=wholeword) #, USE.NAMES=FALSE)
    if (sum(is.na(contexts))==length(contexts)) return(NA) # means no search term found
    # name the text vector
    if (!is.null(names(x))) {
        names(contexts) <- names(x)
    } else {
        names(contexts) <- paste("text", 1:length(x), sep="")
    }
    contexts <- contexts[!is.na(contexts)]
    for (l in 1:length(contexts)) {
        contexts[[l]]$source <- paste("[", names(contexts[l]), ", ",  contexts[[l]]$source, "]", sep="")
    }
    contexts <- do.call("rbind", contexts)
    contexts$source <- format(contexts$source, justify="right")
    contexts$postword <- format(contexts$postword, justify="left")
    row.names(contexts) <- contexts$source
    contexts <- contexts[,-1]   
    return(contexts)
}

#' @rdname kwic
#' @method kwic corpus
#' @export 
kwic.corpus <- function(x, word, window=5, wholeword=FALSE) {
    return(kwic(texts(x), word, window, wholeword))
}

