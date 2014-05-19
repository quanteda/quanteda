library(tm)
library(stm)
library(quanteda)
data(iebudgets)
d <- dfm(iebudgets, stopwords=TRUE)
docs <- readCorpus(d, type=c("dtm"))
vocab <- colnames(d)

x <- stm(docs$documents, docs$vocab, 15)