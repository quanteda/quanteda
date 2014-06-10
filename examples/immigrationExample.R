library(quanteda)
library(topicmodels)
library(lda)
library(ca)
data(stopwords)
custom_stopwords <- c('daily','telegraph', '-', 'times','sun', 'guardian', 'said','independent',
                      '£', 'newspaperdaily', 'dailystarcouk', 'newspaperdailystarcoukid', 'mailid',
                      'came', '–', 'mr', 'also', 'can','now', 'x',
                      'im', 'immigration','immigrants','immigrant',
                      'will', 'people', 'one')
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
ldaModel <- LDA(trimByPaperTm, control = list(alpha = 0.1), k = 20)
get_terms(ldaModel, k=15)


model <- ca(t(trimByPaper), nd = 1)
model2 <- ca(t(trimByPaper), nd = 2)

dotchart(model$colcoord[order(model$colcoord[,1]),1], labels = model$colnames[order(model$colcoord[,1])])



plot(model2,what=c("none","active"))


ldaResult <- lda.collapsed.gibbs.sampler(documents=td$documents, 
                                                     K=3,  
                                                     vocab=td$vocab,
                                                     num.iterations=50, alpha=0.1, eta=0.1) 

top.topic.words(ldaResult, 10, by.score=TRUE) # top five words in each topic


if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)
