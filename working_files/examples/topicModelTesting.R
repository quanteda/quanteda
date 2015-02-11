library(quanteda)
library(topicmodels)
library(lda)
library(ca)
data(stopwords)
custom_stopwords <-
  
  
custom_stopwords <- c(custom_stopwords, stopwords$english)

path = '/home/paul/Dropbox/LSETextMining/code/articles'
attNames = c("paperName", "id")
newsCorpus <- corpusFromFilenames(path, attNames, sep = "_")
paperCount <- table(newsCorpus$attribs$paperName)
topPapers<- names(sort(paperCount, decreasing = TRUE)[1:21])
reducedCorpus <- subset(newsCorpus, paperName %in% topPapers)

reducedDfm <- dfm(reducedCorpus)
reducedByPaperDfm <- dfm(newsCorpus, group = "paperName")
reducedDfmStopwords <- stopwordsRemove(reducedByPaperDfm, custom_stopwords)


trimByPaper <- dfmTrim(reducedDfmStopwords, minCount=1, minDoc=2) 
trimByPaperTm<- dfm2tmformat(trimByPaper)