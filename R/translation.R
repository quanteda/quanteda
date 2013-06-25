# helper function for directly calling the translate API
# sourceText must be 1000 characters or less
# the rate limit is allegedly 1000 queries per day
translateChunk <- function(sourceText, sourceLanguage, targetLanguage, key=NULL, verbose=FALSE) {
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


#' Send a corpus to the google translate research API

#' This function translates a the texts in a corpus by sending them
#'  to the google translate API.
#'  
#' @param corpus corpus to be translated
#' @param targetlanguageString Language of the source text
#' @param languagevar Language of the translated text
#' @export
#' @examples
#' translation <- translate(original, fr, de, key='insertkeyhere')
translate.corpus <- function(corpus, targetlanguageString, 
                             textvar="texts", languagevar="language") {
  ## function to translate the text from a corpus into another language
  ## wrapper for translate
  # initialize the translated text vector
  translatedTextVector <- rep(NA, nrow(corpus$attribs))
  for (i in 1:nrow(corpus$attribs)) {
    if (corpus$attribs[i,textvar]=="" | is.na(corpus$attribs[i,textvar])) next
    if (corpus$attribs[i,languagevar]==targetlanguageString) next
    translatedTextVector[i] <- translate(corpus$attribs[i,textvar], 
                                         corpus$attribs[i,languagevar],
                                         targetlanguageString)
  }
  return(translatedTextVector)
}

#' Send text to the google translate research API

#' This function translates a text by sending it to the google translate API.
#'
#'  
#' @param sourceText Text to be translated
#' @param sourceLanguage Language of the source text
#' @param targetLanguage Language of the translated text
#' @param key API key for Google Translate research API
#' @export
#' @examples
#' translation <- translate(original, fr, de, key='insertkeyhere')
translate <- function(sourceText,  sourceLanguage, targetLanguage, key=NULL, verbose=FALSE){
  a <- strsplit(sourceText, split="[\\.]")
  sentences <- unlist(a)
  # Paste sentences together into a chunk until the next one would send the current chunk
  # over 1000 chars, then send to Google.
  chunk <- ""
  translatedText <- ""
  for (i in 1:length(sentences)) {
    s <- sentences[i]
    if (nchar(s) < 2) {
      if(verbose) print("empty sentence")
      next
    }
    s <- curlEscape(s)
    
    # handle the rare (non-existent?) case of a single sentence being >1000 chars
    if (nchar(s) >= 1000) {
      if (verbose) print("in the 1000 case")
      start <- 1
      end <- 1000
      while ((nchar(s) - start) > 1000) {
        chunk <- substr(s, start, end)
        translatedText <- paste(translatedText, translateChunk(chunk,sourceLanguage, targetLanguage,key), sep=". ")
        start <- start + 1000
        end <- end + 1000
      }
      chunk <- substr(s, start, nchar(s))
      translatedText <- paste(translatedText,translateChunk(chunk,sourceLanguage, targetLanguage,key), sep="")
      chunk <- ""
    }
    else {
      # if this is the last sentence in the speech,
      # send it and the current chunk (if there is one) to Google
      if (i==length(sentences)) {
        if (verbose) print("one")
        #send to Google, reset the chunk
        if (nchar(chunk)>5) {
          translatedText <- paste(translatedText, translateChunk(chunk, sourceLanguage, targetLanguage,key),sep=". ")
        }else{
          if (verbose) print("empty chunk")
        }
        if (nchar(s)>5) {
          translatedText <- paste(translatedText, translateChunk(s, sourceLanguage, targetLanguage,key),sep=". ")
        } else {
          if (verbose) print("empty sentence")
        }
        chunk <- ""
      }
      #if this sentence will put the chunk over 1000, send the chunk to 
      #Google and save this sentence
      else if ((nchar(chunk)+nchar(s) >= 1000)) {
        if (verbose) print("two")
        translatedText <- paste(translatedText, translateChunk(chunk, sourceLanguage, targetLanguage, key), sep=". ")
        chunk <- paste(s,".%20",sep="")
      } else {
        if (verbose) print("three")
        #otherwise just add this sentence to the chunk
        chunk <- paste(chunk, s, sep=".%20")
      }
    }
  }
  translatedText <- curlUnescape(translatedText)
  if (verbose) cat("****************", translatedText, "********************", nchar(translatedText), "\n")
  if (verbose) cat("\n")
  return(translatedText)
}
