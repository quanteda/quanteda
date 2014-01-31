getWordStatCSV <-
function(filename=NULL) {
  f <- ifelse(is.null(filename), file.choose(), filename)
  doc <- read.csv(f, stringsAsFactors=FALSE)
  text <- doc$DOCUMENT
  atts <- subset(doc, select=-c(DOCUMENT))
  c <- corpus.create(text, attribs=atts)
  return(c)
}
