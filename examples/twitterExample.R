library(twitteR)

library(RCurl)
load("~/Dropbox/QUANTESS/social media/QUANTESS_oauth.Rdata")
key='ZOHJIRAwnw23FhvFWyUg'
cons_secret='HTfEcEmRRDcx0ZsJ5CHOcmPc84AfDOp5VvIXwt0oY'
token='778251283-ZkDTfl3IbIFZFXlVokA6Gpc19TZPyov3wucZ0XaB'
access_secret='8vgPnpEWP3qhvILmTLXVb9RslwcEwVVeKOo4KCYHOY'
x <- twitterTerms('rstudio', numResults=50, key, cons_secret, token, access_secret)