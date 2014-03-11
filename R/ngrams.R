
#' Create bigrams
#' 
#' @param tokens
#' @author kohei Watanabe
#' @return a character of bigrams vector
#' @examples
#' bigrams(c("aa", "bb", "cc", "dd", "ee", "ff"))
bigrams <- function(tokens){
  t <- tokens
  #print(w)
  l1 <- length(t)
  m1 <- append(t[2:l1], '')
  m2 <- t
  #print(paste(m1, m2, sep='-'))
  m3 <- append('', t[1:l1-1])
  b <- c(paste(m3, m2, sep='-'), paste(m2, m1, sep='-'))
  l2 <- length(b)
  #print(b)
  bigrams <- paste(b[3:l2-1], sep='')
  return(bigrams)
}