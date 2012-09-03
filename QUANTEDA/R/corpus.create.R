corpus.create <-
function(texts, textnames=NULL, attribs=NULL, source=NULL, notes=NULL, attribs.labels=NULL) {
  if (is.null(names(texts))) 
    names(texts) <- paste("text", 1:length(texts), sep="")
  if (is.null(source)) 
    source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
  created <- date()
  metadata <- c(source=source, created=created, notes=notes)
  if (!is.null(attribs)) {
    attribs <- data.frame(texts=texts,
                          attribs,
                          row.names=names(texts), 
                          check.rows=TRUE, stringsAsFactors=FALSE)
  }
  if (!is.null(attribs) & is.null(attribs.labels)) {
    attribs.labels <- c("Original texts", rep(NULL, length(attribs)-1))
  }
  temp.corpus <- list(attribs=attribs,
                      attribs.labels=attribs.labels,
                      metadata=metadata)
  class(temp.corpus) <- list("corpus", class(temp.corpus))
  return(temp.corpus)
}
