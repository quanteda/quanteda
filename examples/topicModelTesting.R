library(tm)
library(stm)
library(quanteda)
data(movies)
data(stopwords)
d <- dfm(movies, stopwords=TRUE)
docs <- readCorpus(d, type=c("dtm"))
vocab <- colnames(d)

x <- stm(docs$documents, docs$vocab, 5, max.em.its=20)
labelTopics(x)

if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)

