library(quanteda)
library(topicmodels)
library(lda)
path = '/home/paul/Dropbox/LSETextMining/code/articles'
attNames = c("paperName", "id")
newsCorpus <- corpusFromFilenames(path, attNames, sep = "_")
paperCount <- table(newsCorpus$attribs$paperName)
topPapers<- names(sort(paperCount, decreasing = TRUE)[1:21])
reducedCorpus <- subset(newsCorpus, paperName%in%topPapers)
data(stopwords)
fulld <- dfm(newsCorpus, stopwords=stopwords$english)
fullByPaper <- dfm(newsCorpus, group = "paperName", stopwords=stopwords$english)
dtmReduced <- dfmTrim(fulld, minCount=1, minDoc=10) 
td <- dfm2tmformat(dtmReduced)
tdByPaper <- - dfm2tmformat(dtmByPaper)
ldaModel <- LDA(td, control = list(alpha = 0.1), k = 10)
get_terms(ldaModel, k=5)

ldaModelbyPaper <- LDA(fullByPaper, control = list(alpha = 0.1), k = 10)
get_terms(ldaModelbyPaper, k=5)

library(ca)
model <- ca(t(fullByPaper), nd = 1)


ldaResult <- lda.collapsed.gibbs.sampler(documents=td$documents, 
                                                     K=3,  
                                                     vocab=td$vocab,
                                                     num.iterations=50, alpha=0.1, eta=0.1) 

top.topic.words(ldaResult, 10, by.score=TRUE) # top five words in each topic


if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)
