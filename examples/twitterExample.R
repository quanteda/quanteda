library('twitteR')
library(quanteda)
library(RCurl)


key = 'your consumer key here'
cons_secret = 'your consumer secret here'
token = 'your access token here'
access_secret = 'your access secret here'
x <- twitterTerms('rstudio', numResults=50, key, cons_secret, token, access_secret)

