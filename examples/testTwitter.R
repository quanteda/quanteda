library(twitteR)
library(quanteda)
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")
x <- twitterTerms('rstudio',  my_oauth, 17)