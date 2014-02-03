
#' Imports a Wordstat corpus from a CSV file
#'
#' Reads in a wordstat CSV file and creates a corpus object
#' with the document as text and variables as attributes
#' @export
#' 
#' 
getWordStatCSV <- function(filename=NULL) {
  f <- ifelse(is.null(filename), file.choose(), filename)
  doc <- read.csv(f, stringsAsFactors=FALSE)
  text <- doc$DOCUMENT
  atts <- subset(doc, select=-c(DOCUMENT))
  c <- corpus.create(text, attribs=atts)
  return(c)
}


#' Imports a Wordstat corpus from an XML file
#'
#' Reads in a wordstat XML file and creates a corpus object
#' with the document as text and variables as attributes
#' @export
#' 
getWordStat <- function(filename=NULL) {
  require(XML)
  f <- ifelse(is.null(filename), file.choose(), filename)
  str <- paste(readLines(f), collapse=" ")
  str <- gsub('<codebook>','',str)
  str <- gsub('</codebook>','',str)
  doc <- xmlRoot(xmlTreeParse(str))
  # the first two nodes are description and codebook - structure holds all the cases and vars
  struct <- doc[['structure']]
  
  # the variables are declared before the cases but at the same level - have to filter them out
  vars <- Filter(function(x) xmlName(x)=="variable", xmlChildren(struct))
  children <- Filter(function(x) xmlName(x)=="case", xmlChildren(struct))
  texts <- rep(NA, length(children))
  init <- rep(NA, length(children[[1]])-1)
  atts <- as.data.frame(t(init))
  
  # for each case we take out the document and the variable values
  for(i in 1:length(children)){
    x <- children[[i]]
    vals <- sapply(x, xmlValue)
    ns <- sapply(x, xmlName)
    names(vals) <- ns
    texts[[i]] <- vals['DOCUMENT']
    vals <- x[-which(names(x)=='DOCUMENT')]
    vals <- sapply(vals, xmlValue)
    names(atts) <- names(vals)
    atts <- rbind(atts, vals)
    atts <- na.omit(atts)
  }
  c <- corpus.create(texts, attribs=atts)
  return(c)
}
