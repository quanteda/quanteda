##' List key words in context from a text or a corpus of texts.
##'
##' For a text or a collection of texts (in a quanteda corpus object), return a list
##' of a keyword supplied by the user in its immediate context, identifying the source
##' text and the word index number within the source text.  (Not the line number, since
##' the text may or may not be segmented using end-of-line delimiters.)
##' 
##' @param text A text character scalar or a quanteda corpus.  (Currently does not support character vectors.)
##' @param word A keyword chosen by the user.
##' @param window The number of context words to be displayed around the keyword.
##' @param regex If TRUE (default), then "word" is a regular expression, otherwise only match the whole word.
##' Note that if regex=TRUE and no special regular expression characters are used in the search query, 
##' then the concordance will include all words in which the search term appears, and not just when it
##' appears as an entire word.  (For instance, searching for the word "key" will also return "whiskey".)
##' @return A data frame with the context before (\code{preword}), the keyword in its original format (\code{word}, preserving case and attached punctuation), and the context after (\code{postword}).  The rows of the
##' dataframe will be named with the word index position, or the text name and the index position
##' for a corpus object.  
##' @author Kenneth Benoit and Paul Nulty
##' @export
##' @examples
##' kwic(inaugTexts, "terror")
##' kwic(inaugTexts, "terror", regex=FALSE)  # returns only whole word, without trailing punctuation
##' data(iebudgets)
##' kwic(subset(iebudgets, year==2010), "Christmas", window=4) # on a corpus
kwic <- function(text, word, window=5, regex=TRUE) {
    UseMethod("kwic")
}

kwicSingleText <- function(text, word, window=5, regex=TRUE) {
    # don't use tokenize since we want to preserve case and punctuation here
    # grep needed to get words that end in punctuation mark or in quotes
    tokens <- strsplit(text, " ")[[1]]
    # interpret word as a regular expression if regex==TRUE
    match.expression <- ifelse(regex, 
                                tolower(word),
                                paste("^", tolower(word), "$", sep=""))
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
#' @param texts a vector of texts
#' @rdname kwic
#' @S3method kwic character
kwic.character <- function(texts, word, window=5, regex=TRUE) {
    contexts <- lapply(texts, kwicSingleText, word=word, window=window, regex=regex) #, USE.NAMES=FALSE)
    if (sum(is.na(contexts))==length(contexts)) return(NA) # means no search term found
    # name the text vector
    if (!is.null(names(texts))) {
        names(contexts) <- names(texts)
    } else {
        names(contexts) <- paste("text", 1:length(texts), sep="")
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
#' @param corp a quanteda corpus object
#' @rdname kwic
#' @S3method kwic corpus
kwic.corpus <- function(corp, word, window=5, regex=TRUE) {
    return(kwic(texts(corpus), word, window, regex))
}

