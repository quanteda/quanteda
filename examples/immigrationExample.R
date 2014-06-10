library(quanteda)
library(topicmodels)
library(lda)
library(ca)
path = '/home/paul/Dropbox/LSETextMining/code/articles'
attNames = c("paperName", "id")
newsCorpus <- corpusFromFilenames(path, attNames, sep = "_")
paperCount <- table(newsCorpus$attribs$paperName)
topPapers<- names(sort(paperCount, decreasing = TRUE)[1:21])
reducedCorpus <- subset(newsCorpus, paperName %in% topPapers)
data(stopwords)
reducedDfm <- dfm(reducedCorpus, stopwords=stopwords$english)
reducedByPaper <- dfm(newsCorpus, group = "paperName", stopwords=stopwords$english)
trimByPaper <- dfmTrim(fulld, minCount=1, minDoc=2) 
trimByPaperTm<- dfm2tmformat(trimByPaper)
ldaModel <- LDA(trimByPaperTm, control = list(alpha = 0.1), k = 10)
get_terms(ldaModel, k=5)
model <- ca(t(trimByPaper), nd = 2)


ldaResult <- lda.collapsed.gibbs.sampler(documents=td$documents, 
                                                     K=3,  
                                                     vocab=td$vocab,
                                                     num.iterations=50, alpha=0.1, eta=0.1) 

top.topic.words(ldaResult, 10, by.score=TRUE) # top five words in each topic


if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)
