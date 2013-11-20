library(twitteR)
library(quanteda)
library(austin)
source("/home/paul/Dropbox/code/quanteda/R/corpustools.R")
source("/home/paul/Dropbox/code/quanteda/R/languagetools.R")
load("~/Dropbox/QUANTESS/social media/twitterStreaming/my_oauth.Rdata")

results <- sapply(twitterTerms('rstudio', my_oauth), as.data.frame)
