corpus.create <-
function(texts, textnames=NULL, attribs=NULL, source=NULL, notes=NULL, attribs.labels=NULL) {
  if (is.null(names(texts))) 
    names(texts) <- paste("text", 1:length(texts), sep="")
  if (is.null(source)) 
    source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
  created <- date()
  metadata <- c(source=source, created=created, notes=notes)
  

  
  if (is.null(attribs)) {
    attribs <- data.frame(texts, row.names=names(texts), 
                          check.rows=TRUE, stringsAsFactors=FALSE)
  } else attribs <- data.frame(texts, attribs,
                               row.names=names(texts), 
                               check.rows=TRUE, stringsAsFactors=FALSE)
  temp.corpus <- list(attribs=attribs,
                      metadata=metadata)
  class(temp.corpus) <- list("corpus", class(temp.corpus))
  return(temp.corpus)
}
