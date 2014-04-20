#' This file is a walkthrough for getting access to the twitter developer API. It used to be possible
#' to do this with just a handle and password, but now OAuth is required.

#' 1. Go to https://dev.twitter.com/ and sign in with a twitter account
#' 2. Go to My Applications and select "New App". https://apps.twitter.com/
#' 3. Fill out the form and click 'create'
#' 


library(ROAuth)
# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "http://api.twitter.com/oauth/access_token"
# authURL <- "http://api.twitter.com/oauth/authorize"
# consumerKey <- "mykey"
# consumerSecret <- "mysecret"
# my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
#                              requestURL = requestURL, accessURL = accessURL, authURL = authURL)
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# save(my_oauth, file = "my_oauth.Rdata")


twitterHandshake(consumerKey, consumerSecret){
  
}