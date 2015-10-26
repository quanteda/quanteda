
require(Rcpp)
Rcpp::sourceCpp('src/ngrams.cpp')

#' @rdname ngrams_c
ngrams_c <- function(x, n=2, concatenator="_"){
    ngramcpp(x, n, 1, concatenator)
}

skipgrams_c <- function(x, n, skip, concatenator="_"){
  skipgramcpp(x, n, 1 + skip, concatenator)
}
# 
# # For a vector of characters  ---------------------------------------
# 
# ngrams_c(head(LETTERS), n = 3) #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
# ngrams(head(LETTERS), n = 3) #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
# 
# ngrams_c(head(LETTERS), n = 3) #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
# ngrams(head(LETTERS), n = 3) #"A_B_C" "B_C_D" "C_D_E" "D_E_F"
# ngrams_c(head(LETTERS), n = 3) #"A_C_E" "B_D_F"
# ngrams(head(LETTERS), n = 3) #"A_C_E" "B_D_F"
# ngrams_c(head(LETTERS), n = 3) # None
# ngrams(head(LETTERS), n = 3) # None
# 
# skipgrams_c(head(LETTERS), n = 2, skip = 1) #"A_B" "B_C" "C_D" "D_E" "E_F" "A_C" "B_D" "C_E" "D_F"
# skipgrams(head(LETTERS), n = 2, k = 1)   #"A_B" "B_C" "C_D" "D_E" "E_F" "A_C" "B_D" "C_E" "D_F"
# 
# skipgrams_c(head(LETTERS), n = 2, skip = 2) #"A_B" "B_C" "C_D" "D_E" "E_F" "A_C" "B_D" "C_E" "D_F" "A_D" "B_E" "C_F"
# skipgrams(head(LETTERS), n = 2, k = 2)   #"A_B" "B_C" "C_D" "D_E" "E_F" "A_C" "B_D" "C_E" "D_F" "A_D" "B_E" "C_F"
# 
# # For a list of vectors of characters ---------------------------
# 
# data(inaugTexts)
# texts <- tokenize(toLower(inaugTexts))
# ngrams <- ngramcppl(texts, 3, 1, '+')
# skipgrams <- skipgramcppl(texts, 3, 3, '+')
# 
# # Speed test
# data(inaugTexts)
# system.time(
#   out <- lapply(texts, function(text) skipgramcpp(text, 5, 5, '+'))
# )
# system.time(
#   out <- skipgramcppl(texts, 5, 5, '+')
# )  
# 
