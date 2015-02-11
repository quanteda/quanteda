
library(austin)


# get the year, case id, and judge(s) names from the filname
getParts <- function(casename){
  casename <- sub('.txt','', casename)
  len <- nchar(casename)
  year <- substr(casename, 1, 4)
  x <- substr(casename,10,10)
  if(!is.na(as.numeric(x))) {
    judge <- substr(casename, 11, len)
    id <- substr(casename,5,11)
  }else{
    judge <- substr(casename,10,len)
    id <- substr(casename,5,9)
  }
  return(c(year, id, judge))
}

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/UkSupreme/2011-12cases")
parts <- (sapply(names(texts), getParts))
parts <- t(parts)
# get just the filenames
filenames <- parts[,3]
#get a list of unique attributes(judges)
judges <- unlist(lapply(filenames, strsplit, '[,&]'))
judges <- unique(judges)
all_atts <- append(judges, c('year', 'id'))
newattribs <-  data.frame((all_atts), nrow=length(all_atts))