translate <-
function(sourceText, sourceLanguage, targetLanguage, key=NULL, verbose=FALSE) {
  require(RCurl)
  require(XML)
  if (is.null(key)) stop("Error: Must supply a key for Google Translate.")
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
        translatedText <- paste(translatedText, translateChunk(chunk,sourceLanguage, targetLanguage, key), sep=". ")
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
  
  # the joining code introduces an initial full-stop, remove it.
  translatedText <- sub(pattern="\\. ", replacement="",translatedText)
  if (verbose) cat("****************", translatedText, "********************", nchar(translatedText), "\n")
  if (verbose) cat("\n")
  return(translatedText)
}
