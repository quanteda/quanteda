#' cfm performance test
#' 
#' @param x tokenizedTexts
#' @export
#' @examples 
#' devtools::install_github("kbenoit/quantedaData")
#' txt <- subset(inaugCorpus, Year > 1900)
#' data(SOTUCorpus, package = "quantedaData")
#' txt<-SOTUCorpus
#' microbenchmark::microbenchmark(
#'                              text2vecPfm(txt),
#'                              cfm3Pfm(txt),
#'                              cfmPfm(txt),
#'                              cfm4Pfm(txt),
#'                              times = 1,
#'                              unit = 'relative') 
#' microbenchmark::microbenchmark(
#'                              text2vecPfm(tokens,vectorizer),
#'                              cfmPfm(toks),
#'                              cfm4numericToCpp(toks),
#'                              cfm2numericToken(numericX),
#'                              times = 1,
#'                              unit = 'relative')                              
tokens <- txt %>% tolower %>% word_tokenizer
it <- itoken(tokens)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)

toks <- tokenize(toLower(txt), removePunct = TRUE)

types <- unique(unlist(toks, use.names = FALSE))
types <- sort(types)
numericX<- lapply(toks, function(x, y) match(x, y), types)

text2vecPfm <- function(tokens,vectorizer ){
    #tokens <- txt %>% tolower %>% word_tokenizer
    #it <- itoken(tokens)
    #v <- create_vocabulary(it)
    #vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
    tcm <- create_tcm(itoken(tokens), vectorizer)
    return(tcm)
}

cfmPfm <- function(txt){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
  #toks_index <- lapply(toks, function(x, y) match(x, y), types)
    cfm <- cfm(toks, context = "window", count = "weighted", window = 3)
    return(cfm)
}

cfmPfmB <- function(txt){
    
    cfm <- cfm(toks, context = "window", count = "boolean", window = 3)
    return(cfm)
}
cfm3Pfm <- function(toks){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
    cfm3 <- cfm3(toks, context = "window", count = "weighted", ordered = TRUE, window = 3)
    return(cfm3)
}

cfm4numericToCpp <- function(toks){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
    cfm4 <- cfm4(toks, context = "window", count = "weighted", ordered = TRUE, window = 3)
    return(cfm4)
}

cfm2numericToken <- function(numericX){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
    #types <- unique(unlist(toks, use.names = FALSE))
    #types <- sort(types)
    #numericX<- lapply(toks, function(x, y) match(x, y), types)
    cfm2 <- cfm2.indexedTexts(numericX, context = "window", count = "weighted", ordered = TRUE, window = 3)
    return(cfm4)
}


#|=====================================================================================================| 100%Unit: relative
##inaugCorpus
# : relative
#               expr       min        lq      mean    median        uq       max neval
#text2vecPfm(tokens)     2.543569  2.543569  2.543569  2.543569  2.543569  2.543569     1
#cfmPfm(toks)           18.058517 18.058517 18.058517 18.058517 18.058517 18.058517     1
#cfm4numericToCpp(toks) 17.349280 17.349280 17.349280 17.349280 17.349280 17.349280     1
#cfm2numericToken(toks)  1.000000  1.000000  1.000000  1.000000  1.000000  1.000000     1

#|=====================================================================================================| 100%Unit: relative
# quantedaData
#relative
# expr                              min        lq      mean    median        uq       max
# cfm3Pfm(toks)                     44.372591 44.372591 44.372591 44.372591 44.372591 44.372591
# text2vecPfm(tokens, vectorizer)   3.347731  3.347731  3.347731  3.347731  3.347731  3.347731
# cfmPfm(toks)                      32.027275 32.027275 32.027275 32.027275 32.027275 32.027275
# cfm4numericToCpp(toks)            29.427185 29.427185 29.427185 29.427185 29.427185 29.427185
# cfm2numericToken(numericX)        1.000000  1.000000  1.000000  1.000000  1.000000  1.000000