library('twitteR')
library(quanteda)
library(RCurl)
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")
key = 'your consumer key here'
cons_secret = 'your consumer secret here'
token = 'your access token here'
access_secret = 'your access secret here'
x <- twitterTerms('methods', 10, key, cons_secret, token, access_secret)

