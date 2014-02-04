#library(quanteda)

kwic <- function(text, word, window=5){
  UseMethod("kwic")
}

kwic.character <- function(text, word, window=5) {
    # don't use tokenize since we want to preserve case and punctuation here
    # grep needed to get words that end in punctuation mark or in quotes
    tokens <- strsplit(text, " ")[[1]]
    matches <- grep(paste("?", tolower(word), "?", sep=""), tolower(tokens))
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

kwic.corpus <- function(corpus, word, window=5){
    # need to modify this so that it returns a data frame, and
    # puts "[filename, line XX]" in the "source" column
    contexts <- sapply(corpus$attribs$texts, kwic, word=word)
    names(contexts) <- row.names(corpus$attribs)
    return(contexts)
}

#text <- "Keanu is really \"great\" in this movie, and Arnold is great too.  I think R is a GREAT programming tool for statistics, while Matlab just is not that great."

#t <- kwic(text, "great")

