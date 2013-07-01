# File-Name:       sentence_parser.R
# Date:            2013-07-01
# Author:          Ken Benoit
# Email:           kbenoit@lse.ac.uk                                      
# Purpose:         Parse and translate the sentences in manifestos
# Data Used:       ES_manifestos/*.txt
# Packages Used:   quanteda
# Output File:     
# Data Output:     
# Machine:         Ken Benoit's Macbook Air


#rm(list = ls()) # clear memory
library(quanteda)
setwd("~/Dropbox/QUANTESS/Testing/sentence_parser/")

# load in all texts in named directory
EStexts <- getTextDir("ES_manifestos")
# check current encodings
system("file ES_manifestos/*.txt")
# convert two of the texts into unicode (the third is already unicode)
EStexts[c(1,3)] <- iconv(EStexts[c(1,3)], from="latin1", to="UTF-8", sub="byte")
# create a corpus object
EScorpus <- corpus.create(EStexts, notes="Supplied by Pablo.")
summary(EScorpus)
# reshape into sentence corpus
EScorpus.sentences <- corpus.reshape(EScorpus)
sentence.df <- EScorpus.sentences$attribs