library(quanteda)

skipgrams_c <- function(x, n, skip, concatenator="_"){
  skipgramcpp(x, n, 1 + skip, concatenator);
}

tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."), 
                   removePunct = TRUE, simplify = TRUE)


skipgrams(tokens, n = 2, skip = 1, concatenator = " ")
skipgrams_c(tokens, n = 2, skip = 1, concatenator = " ")

skipgrams(tokens, n = 2, skip = 2, concatenator = " ")
skipgrams_c(tokens, n = 2, skip = 0:2, concatenator = " ")

skipgrams(tokens, n = 3, skip = 2, concatenator = " ")
skipgrams_c(tokens, n = 3, skip = 0:2, concatenator = " ")

skipgrams(tokens, n = 3, skip = 2, concatenator = " ")
skipgrams_c(tokens, n = 3, skip = 0:2, concatenator = " ")
