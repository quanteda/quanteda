translateChunk <-
function(sourceText, sourceLanguage, targetLanguage, key=NULL, verbose=TRUE) {
  if (is.null(key)) {
    key <- "DQAAALoAAABQS8Lok-tdR8rU1ewhKf1o7IJxS0m_X63cVuDI3ETGyg8rgWhgYTyaXBDdqIe1TUSlCzTbUi70iYQ5bsTOznfk_W9yXNG68iKxExrSxyy5iT5nXbRn3dXONOCcqkNHmJJ-zQAmwP4Gw3uyFEx2A_JES5Xru_Kaq2aJ9hOfRae8h4bqN_PKe7T_HRTg0xhwaNVWGno_tctoe5zXOHcRbEeRyFG-TTrD45ceJMSat7NPF2n7noIPFvL9SNpD026RSPM"
  }
  if (verbose){
    cat("Making call to Google Translate..., with string of length: ", nchar(sourceText), "\n")
    print(sourceText)
  }
  baseUrl <- "http://translate.google.com/researchapi/translate?"
  params <- paste("sl=",sourceLanguage, "&tl=", targetLanguage, "&q=", sourceText,sep="")
  
  url <- paste(baseUrl,params,sep="")
  header <- paste("Authorization: GoogleLogin auth=", key, sep="")
  # make the http requst with the url and the authentication header
  if (verbose) print(url)
  curl <- getCurlHandle()
  response <- getURL(url, httpheader=header, curl=curl)
  # get the http response code to try to see what type of error we're getting
  code <- getCurlInfo(curl, which="response.code")
  print(code)
  rm(curl)
  Sys.sleep(1)
  # parse XML response to extract actual translation
  doc <- xmlTreeParse(response, getDTD = F)
  r <- xmlRoot(doc) 
  translation <- xmlValue(r["entry"] [[1]] [[5]])
  return(translation)
}
