library(quanteda)
library(topicmodels)
library(lda)
library(ca)
custom_stopwords <- readLines('/home/paul/Dropbox/LSETextMining/code/stopwords.txt')

path = '/home/paul/Dropbox/LSETextMining/code/articles/'
attNames = c("paperName", "id")

newsCorpus <- corpusFromFilenames(path, attNames, sep = "_")

# filter out smaller papers
paperCount <- table(newsCorpus$attribs$paperName)
topPapers<- names(sort(paperCount, decreasing = TRUE)[1:21])
reducedCorpus <- subset(newsCorpus, paperName %in% topPapers)

# make a dfm and triplet matrix dfm from without grouping (just by doc)
byDocDfm <- dfm(reducedCorpus)
byDocDfmTrim <- dfmTrim(byDocDfm, minCount=3, minDoc=2) 
finalDfmByDoc <- stopwordsRemove(byDocDfmTrim, custom_stopwords)
finalDfmByDoc <- finalDfmByDoc[which(rowSums(finalDfmByDoc) > 0),] 
finalTripletByDoc<- dfm2tmformat(finalDfmByDoc)

ldaByDocVEMNoAlpha30 <- LDA(finalTripletByDoc, method="VEM", k = 30)

ldaByDocVEMNoAlpha25 <- LDA(finalTripletByDoc, method="VEM", k = 25)

ldaByDocVEMNoAlpha15 <- LDA(finalTripletByDoc, method="VEM", k = 15)

ldaByDocVEMNoAlpha8 <- LDA(finalTripletByDoc, method="VEM", k = 8)



get_terms(ldaByDocVEMNoAlpha25, 10)
perplexity(ldaByDocVEMNoAlpha25)


ldaByDocGibbsNoAlpha25 <- LDA(finalTripletByDoc, method="VEM", k = 25)


#CTM models

#culture  = 6
#economic = 24
#jobs = 3
#finance = 28

model <- ldaByDocVEMNoAlpha25
postTopics <- data.frame(posterior(model)$topics)

x <- sapply(rownames(postTopics),strsplit,'_')
paperNames <- sapply(x, head, n=1)
postTopics["paper"]<- paperNames
library(plyr)
byPaperTopics <-ddply(postTopics, "paper", numcolwise(mean))


leftPapers <- c("Guardian", "Independent")
rightPapers <- c("Express", "Mail", 'Times','FT','Telegraph')
keepPapers <- c("Express", "Mail","Guardian", "Independent",'Times','FT','Telegraph')
byPaperTopics<-byPaperTopics[byPaperTopics$paper %in% keepPapers,]


barplot(as.matrix(y), names.arg=byPaperTopics$paper)



barplot(byPaperTopics$X21, names.arg=byPaperTopics$paper)
barplot(byPaperTopics$X24, names.arg=byPaperTopics$paper)
barplot(byPaperTopics$X28, names.arg=byPaperTopics$paper)









# model <- ca(t(finalDfm), nd = 1)
# model2 <- ca(t(finalDfm), nd = 2)
# dotchart(model$colcoord[order(model$colcoord[,1]),1], labels = model$colnames[order(model$colcoord[,1])])
# plot(model2,what=c("none","active"))
# ldaResult <- lda.collapsed.gibbs.sampler(documents=td$documents, 
#                                                      K=3,  
#                                                      vocab=td$vocab,
#                                                      num.iterations=50, alpha=0.1, eta=0.1) 

