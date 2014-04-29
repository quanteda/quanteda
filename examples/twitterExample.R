library('twitteR')
library(quanteda)
library(RCurl)
<<<<<<< HEAD
load("~/Dropbox/QUANTESS/social media/QUANTESS_oauth.Rdata")
<<<<<<< HEAD
x <- twitterTerms('rstudio',  my_oauth, 17)
=======
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")
key = 'your consumer key here'
cons_secret = 'your consumer secret here'
token = 'your access token here'
access_secret = 'your access secret here'
x <- twitterTerms('methods', 10, key, cons_secret, token, access_secret)

>>>>>>> 53bc5670b5324dee763f2f1db749c0acd1e18a5a
=======
key='ZOHJIRAwnw23FhvFWyUg'
cons_secret='HTfEcEmRRDcx0ZsJ5CHOcmPc84AfDOp5VvIXwt0oY'
token='778251283-ZkDTfl3IbIFZFXlVokA6Gpc19TZPyov3wucZ0XaB'
access_secret='8vgPnpEWP3qhvILmTLXVb9RslwcEwVVeKOo4KCYHOY'
x <- twitterTerms('rstudio', numResults=50, key, cons_secret, token, access_secret)
>>>>>>> fb55363480ac0d63235c07faaae5102356d9b236
