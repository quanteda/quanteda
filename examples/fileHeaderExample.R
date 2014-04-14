
library(jsonlite)
data(ieTextsHeaders)
texts <- c()
headerAttribs <- data.frame(stringsAsFactors=FALSE)
for (ht in ieTextsHeaders) {
  lines <- unlist(strsplit(ht, '\n'))
  header <- data.frame(fromJSON(lines[1]), stringsAsFactors=FALSE)
  if (is.null(names(headerAttribs))) {
    attribs <- data.frame(header, stringsAsFactors = FALSE)
  }
  else {
    headerAttribs <- rbind(header, headerAttribs)
  }
  content <- paste(lines[2:length(lines)], collapse='\n')
  texts <- c(texts, content) 
}
corp <- corpusCreate(texts, attribs=headerAttribs)




#corp <- corpusFromHeaders("~/Dropbox/QUANTESS/corpora/withHeader")
