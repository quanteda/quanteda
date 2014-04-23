library(twitteR)

library(RCurl)
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")

x <- twitterTerms('rstudio',  my_oauth, 17)