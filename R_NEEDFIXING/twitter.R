#' make a corpus object from results of a twitter REST search
#'
#' All of the attributes returned by the twitteR
#' library call are included as attributes in the
#' corpus. A oauth key is required, for further
#' instruction about the oauth processs see:
#' https://dev.twitter.com/apps/new
#' and the twitteR documentation
#' 
#' @param query Search string for twitter
#' @param numResults Number of results desired.
#' @param key 'your consumer key here'
#' @param cons_secret 'your consumer secret here'
#' @param token 'your access token here'
#' @param access_secret 'your access secret here'
#' 
#' @export
#' @examples
#' \dontrun{
#' twCorp <- twitterTerms('example', 10, key, cons_secret, token, access_secret)
#' }
twitterTerms <- function(query, numResults=50, key, cons_secret, token, access_secret) {
  library('twitteR')
  setup_twitter_oauth(key, cons_secret, token, access_secret)
  sea <- (searchTwitter(query, numResults))
  results <-  twListToDF(sea)
  atts <-as.data.frame(results[,2:ncol(results)])
  texts <- results$text
  twc <- corpus(texts, attribs=atts)
  return(twc)
}

#' work-in-progress interface to Twitter streaming API
twitterStreamer <- function(){
  
}

#' work-in-progress from-scratch interface to Twitter search API
twitterSearch <- function(){
  
  
}
