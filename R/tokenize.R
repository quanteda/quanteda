#' Split a string into words

#' The input text is split into words by whitespace
#' 
#' @param str String to be tokenized
#' @param langNorm If \code{TRUE} (default), French and German special characters are normalized
#' @param removeDigits If \code{TRUE} (default), digits are removed
#' @param lower If \code{TRUE} (default), string is converted to lowercase
#' @param removePunct If \code{TRUE} (default), punctuation is removed
#' 
#' @return a character vector containing the input text tokens
#' @export
#' @examples
#' testtxt <- "The quick brown fox named Séamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."
#' tokenize(testtxt)
#' tokenize(testtxt, lower=FALSE)
tokenize <- function(str, langNorm=FALSE, removeDigits=TRUE, lower=TRUE, removePunct=TRUE) {
    str <- clean(str, langNorm=langNorm, removeDigits=removeDigits, lower=lower, removePunct=removePunct)
    tokens <- scan(what="char", text=str, quiet=TRUE)
    # flush out "empty" strings caused by removal of punctuation and numbers
    tokens <- tokens[tokens!=""]
    return(tokens)
}