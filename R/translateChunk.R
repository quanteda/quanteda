translateChunk <-
function(sourceText, sourceLanguage, targetLanguage, key=NULL, verbose=FALSE) {
  require(RJSONIO)
  if (is.null(key)) {
    key <- ""
  }
  if (verbose){
    cat("Making call to Google Translate..., with string of length: ", nchar(sourceText), "\n")
    print(sourceText)
  }
  baseUrl <- "https://www.googleapis.com/language/translate/v2?"
  params <- paste("key=", key, "&source=", sourceLanguage, "&target=", targetLanguage, "&q=", sourceText,sep="")
  
  url <- paste(baseUrl,params,sep="")
  # make the http requst with the url and the authentication header
  if (verbose) print(url)
  curl <- getCurlHandle()
  response <- getURL(url, curl=curl)
  # get the http response code to try to see what type of error we're getting
  code <- getCurlInfo(curl, which="response.code")
  if(verbose) print(code)
  rm(curl)
  Sys.sleep(1)
  # parse JSON response to extract actual translation
  doc <- fromJSON(response)
  return(unlist(doc)[[1]])
}
