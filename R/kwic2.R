
#' This function is an alternative KWIC
#' 
#' @param text Texts
#' @param word Word of interest
#' @param window Window span in character
#' @param filter Filter files in texts by regular expression
#' @param location Show location of the word
#' @param case Ignore case
#' @return cfvm2 Collocatons as data frame
#' @author Kohei Watanabent
#' @examples 
#' \dontrun{
#' kwic2(texts, "we", filter = '_2010', location=TRUE)
#' }
kwic2 <- function(texts, word, window = 30, filter = '' , location = TRUE, case = TRUE){
  i <- 0
  names <- names(texts)
  if(filter != ''){
    names <- names[grep(filter, names)]
  }
  for(name in names){
    text  <- texts[[name]]
    text <- gsub("(\r\n|\r|\n)", " ", text)
    locs <- as.vector(gregexpr(word, text, ignore.case = case)[[1]])
    if(locs[1] > 0){     
      for(loc in locs){        
        if(loc - window < 1){
          fill <- paste(rep(' ', 1 + window - loc), collapse='')
          text <- paste(fill, text, sep='')
          loc <- 1 + window
        }
        if(nchar(word) + loc + window > nchar(text)){
          fill <- paste(rep(' ', nchar(word) + loc + window - nchar(text)), collapse='')
          text <- paste(text, fill, sep='') 
        }
        i = i + 1
        if(location){
          cat(sprintf("[%d] %s  @%-6d %s\n", i, substr(text, (loc - window), nchar(word) + loc + window), loc, name))
        }else{
          cat(sprintf("[%d] %s\n", i, substr(text, (loc - window), nchar(word) + loc + window)))
        }
      }
    }
  }
}

