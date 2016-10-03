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
#'                              text2vecPfm(tokens,vectorizer),
#'                              cfm(toks,context="window",tri=FALSE), 
#'                              cfm(toksHashed, context="window",tri=FALSE),
#'                              times = 1,
#'                              unit = 'relative')   
#'                              
#'                                                         
#'   

tokens <- txt %>% tolower %>% word_tokenizer
it <- itoken(tokens)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)

toks <- tokenize(toLower(txt), removePunct = TRUE)
toksHashed <- hashTokens(toks)
hashOnly <- as.list(toksHashed)


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
#    expr           min        lq      mean    median        uq       max       neval
#text2vecPfm()      2.112988 2.112988 2.112988 2.112988 2.112988     2.112988     1
#cfm(toks)          1.305394 1.305394 1.305394 1.305394 1.305394    1.305394     1
#cfm(toksHashed)    1.000000 1.000000 1.000000 1.000000 1.000000    1.000000     1

#|=====================================================================================================| 100%Unit: relative
# quantedaData
#       expr      min       lq     mean        median     uq        max     neval
#text2vecPfm()   1.000000 1.000000 1.000000 1.000000 1.000000   1.000000     1
#cfm(toks)       2.560103 2.560103 2.560103 2.560103 2.560103   2.560103     1
#cfm(toksHashed) 1.563512 1.563512 1.563512 1.563512 1.563512   1.563512     1

