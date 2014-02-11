#' make a corpus object from results of a twitter search
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
twitterTerms <- function(query, oauth, numResults=50) {
  library('twitteR')
  registerTwitterOAuth(my_oauth)
  sea <- (searchTwitter(query, numResults))
  results <- sapply(sea, as.data.frame) # ugly fix due to change in twitteR api
  results <- as.data.frame(results)
  atts <- as.data.frame(results[2:nrow(results),])
  texts <- as.character(results[1,])
  twc <- createCorpus(texts, attribs=t(atts))
  return(twc)
}
