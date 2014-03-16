
#' Create bigrams
#' 
#' @param tokens
#' @author kohei Watanabe
#' @return a character of bigrams vector
#' @examples
#' bigrams(c("aa", "bb", "cc", "dd", "ee", "ff"))
bigrams <- function(text, window = 1){
  t <- unlist(strsplit(text, ' '))
  bigrams <- c()
  w <- 1
  for(w in c(1:window)){

    m1 <- c(rep('', w), t)
    m2 <- c(t, rep('', w))
    b <- paste(m1, m2, sep='-')
    #print(b)
    l <- length(b)
    bigrams <- c(bigrams, paste(b[(w+1):(l-w)], collapse=' '))
    
  }
  bigramText <- paste(bigrams, collapse = ' ')
  return(bigramText)
}
#bigrams("aa bb cc dd ee ff", 5)

