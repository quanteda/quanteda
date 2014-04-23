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
#' @param oauth Oauth key
#' @param numResults Number of results desired.
#' @export
#' @examples
#' \dontrun{
#' twCorp <- twitterTerms('example search', my_oauth, numResults=10)
#' }
twitterTerms <- function(query, numResults=50, key, cons_secret, token, access_secret) {
  library('twitteR')
  setup_twitter_oauth(key, cons_secret, token, access_secret)
  sea <- (searchTwitter(query, numResults))
  results <-  twListToDF(sea)
  atts <-as.data.frame(results[,2:ncol(results)])
  texts <- results$text
  twc <- corpusCreate(texts, attribs=atts)
  return(twc)
}

#' work-in-progress interface to Twitter streaming API
twitterStreamer <- function(){
  
}

#' work-in-progress from-scratch interface to Twitter search API
twitterSearch <- function(){
  
  
}
