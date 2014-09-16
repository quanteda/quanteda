# The main demo file for quanteda

str(uk2010immig)
mycorpus <- corpus(uk2010immig, 
                   docvars=list(party=names(uk2010immig)),
                   notes="Immigration-related sections from UK 2010 party manifestos.")
encoding(mycorpus) <- "UTF-8"
language(mycorpus) <- "english"
summary(mycorpus, showmeta=TRUE)

kwic(mycorpus, "deport", 3)

mydfm <- dfm(mycorpus, stem=TRUE, stopwords=TRUE)
docnames(mydfm)
features(mydfm)
quartz(7,7)
wordcloudDfm(mydfm, "BNP")

# some examples of tokenization and string cleaning
library(quantedaData)
data(exampleString)
tokenize(exampleString)
clean(exampleString)
SnowballC::wordStem(exampleString)

# topic models 
library(topicmodels)
prescorpus <- subset(inaugCorpus, Year>1900)
presdfm <- dfm(prescorpus, stopwords=TRUE, stem=TRUE)
presdfm <- dfmTrim(presdfm, minCount=10, minDoc=5) 
presTriplet <- dfm2tmformat(presdfm)
presLDA <- LDA(presTriplet, method="VEM", k=20)
# which terms contribute most to each topic
get_terms(presLDA, k=15)
# which is the dominant topic for each document
get_topics(presLDA)
# the topic contribution of each topic to each document
postTopics <- data.frame(posterior(presLDA)$topics)

# dictionaries


