library(foreign)
#library(quanteda)
source("~/Dropbox/code/quanteda/R/corpustools.R")
source("~/Dropbox/code/quanteda/R/languagetools.R")
sentences <- read.dta("~/Dropbox/code/data/sentence_level_data.dta")

#if i don't declare the encoding like this nchar fails("invalid multibyte string)
Encoding(sentences$sentence_text) <- "UTF-8"
sentences <- subset(sentences, !is.na(sentences$gold_position))
sentences <- subset(sentences, !gold_position=="Neither")
sentences <- subset(sentences, nchar(sentences$sentence_text) > 4)[1500:3500,]

#attribs are everything except the text
new_attribs <- data.frame(sentences[, -3])

c <- corpus.create(sentences$sentence_text, attribs=new_attribs)
fc <- create.fvm.corpus(c)
fc <- subset(fc, rowSums(fc)>2)
gold <- as.character(sentences$gold_position)
create.arff(fc, gold)