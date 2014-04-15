#' Split a string into words

#' The input text is split into words by whitespace
#' 
#' @param str String to be tokenized
#' @return a character vector containing the input text cleaned and split by whitespace
#' @export
#' @examples
#' tokens <- tokenize("this is a test")
tokenize <- function(str, lower=TRUE){
  str <- clean(str, lower=lower)
  tokens <- scan(what="char", text=str, quiet=TRUE)
  # flush out "empty" strings caused by removal of punctuation and numbers
  tokens <- tokens[tokens!=""]
  return(tokens)
}