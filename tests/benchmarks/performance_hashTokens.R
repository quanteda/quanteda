#' hashTokens.R performance test
#' 
#' @param x tokenizedTexts
#' @export
#' @examples 
#' devtools::install_github("kbenoit/quantedaData")
#' txt <- subset(inaugCorpus, Year > 1900)
#' data(SOTUCorpus, package = "quantedaData")
#' txt<-SOTUCorpus 
#'
#' microbenchmark::microbenchmark( for(i in 1:100) lapply(toks, function(x,y) fmatch(x,y), types),
#'                              for(i in 1:100) mclapply(toks, function(x,y) fmatch(x,y), types),
#'                              for(i in 1:100) lapply(toks, function(x,y) match(x,y), types),
#'                              for(i in 1:100) mclapply(toks, function(x,y) match(x,y), types),
#'                              times = 1,
#'                              unit = 'relative')                              

library(fastmatch)
library(parallel)
library(microbenchmark)
toks <- tokenize(char_tolower(txt), removePunct = TRUE)
types <- unique(unlist(toks, use.names = FALSE))

library(hash)
######### test results on SOTUCorpus -- hashing time
#expr                    min          lq         mean    median        uq       max neval
#lapply+fmatch      1.000000     1.000000    1.000000  1.000000  1.000000  1.000000     1
#mclapply+ fmatch   5.179605     5.179605    5.179605  5.179605  5.179605  5.179605     1
#lapply+ match      55.948805   55.948805   55.948805 55.948805 55.948805 55.948805     1
#mclapply+ match    38.189749   38.189749   38.189749 38.189749 38.189749 38.189749     1
