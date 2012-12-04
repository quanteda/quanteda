library(foreign)
library(quanteda)
sentences <- read.dta("~/Dropbox/QUANTESS/FOR PAUL 2012-11-30/sentence_level_data.dta")

#if i don't declare the encoding like this nchar fails("invalid multibyte string)
Encoding(sentences$sentence_text) <- "UTF-8"