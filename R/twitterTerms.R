twitterTerms <-
function(query, oauth, numResults=50){
  library('twitteR')
  registerTwitterOAuth(oauth)
  sea <- (searchTwitter(query, numResults))
  atts <- results[2:nrow(results),]
  twc <- corpus.create(texts, attribs=t(atts))
  return(twc)
}
