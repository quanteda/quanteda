library('twitteR')
library(quanteda)
library(RCurl)
<<<<<<< HEAD
load("~/Dropbox/QUANTESS/social media/QUANTESS_oauth.Rdata")
x <- twitterTerms('rstudio',  my_oauth, 17)
=======
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")
key = 'your consumer key here'
cons_secret = 'your consumer secret here'
token = 'your access token here'
access_secret = 'your access secret here'
x <- twitterTerms('methods', 10, key, cons_secret, token, access_secret)

>>>>>>> 53bc5670b5324dee763f2f1db749c0acd1e18a5a
