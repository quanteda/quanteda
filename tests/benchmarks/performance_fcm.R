#' fcm performance test
#' 
#' @param x tokenizedTexts
#' @export
#' @examples 
#' devtools::install_github("kbenoit/quantedaData")
#' txt <- subset(inaugCorpus, Year > 1900)
#' data(SOTUCorpus, package = "quantedaData")
#' txt<-SOTUCorpus

#' microbenchmark::microbenchmark(
#'                              text2vecPfm(tokens,vectorizer),
#'                              fcm(toks,context="window",window = 5L, tri = FALSE), 
#'                              fcm(toksh, context="window",window = 5L,tri = TRUE),
#'                              times = 1,
#'                              unit = 'relative')   
#'                              
#'                                                         
#'   

tokens <- txt %>% tolower %>% word_tokenizer
it <- itoken(tokens)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 5L)

toks <- tokenize(toLower(txt), removePunct = TRUE)
toksh <- tokens(toLower(txt), removePunct = TRUE)


text2vecPfm <- function(tokens,vectorizer ){
    #tokens <- txt %>% tolower %>% word_tokenizer
    #it <- itoken(tokens)
    #v <- create_vocabulary(it)
    #vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
    tcm <- create_tcm(itoken(tokens), vectorizer)
    return(tcm)
}

fcmPfm <- function(txt){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
  #toks_index <- lapply(toks, function(x, y) match(x, y), types)
    fcm <- fcm(toks, context = "window", count = "weighted", window = 3)
    return(fcm)
}

fcmPfmB <- function(txt){
    
    fcm <- fcm(toks, context = "window", count = "boolean", window = 3)
    return(fcm)
}
# |=====================================================================================| 100%Unit: relative
# expr      min       lq     mean   median
# text2vecPfm(tokens, vectorizer) 1.000000 1.000000 1.000000 1.000000
# fcm(toks, context = "window", window = 5L, tri = FALSE) 1.732574 1.732574 1.732574 1.732574
# fcm(toksh, context = "window", window = 5L, tri = TRUE) 1.070455 1.070455 1.070455 1.070455
# uq      max neval
# 1.000000 1.000000     1
# 1.732574 1.732574     1
# 1.070455 1.070455     1
