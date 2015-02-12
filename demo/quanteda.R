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
# open a nicer quartz device on a mac
if (Sys.info()[1]=="Darwin") quartz("BNP 2010 word cloud", 7,7)
# this is necessary currently because no subset is yet implemented for dfm objects
plot(dfm(subset(mycorpus, party=="BNP"), stem=TRUE, stopwords=TRUE))

# some examples of tokenization and string cleaning
library(quantedaData)
data(exampleString)
tokenize(exampleString)
clean(exampleString)
wordstem(exampleString)

# topic models 
library(topicmodels)
prescorpus <- subset(inaugCorpus, Year>1900)
presdfm <- dfm(prescorpus, stopwords=TRUE, stem=TRUE)
presdfm <- trim(presdfm, minCount=10, minDoc=5) 
presTriplet <- dfm2tmformat(presdfm)
presLDA <- LDA(presTriplet, method="VEM", k=20)
# which terms contribute most to each topic
get_terms(presLDA, k=15)
# which is the dominant topic for each document
get_topics(presLDA)
# the topic contribution of each topic to each document
postTopics <- data.frame(posterior(presLDA)$topics)

# dictionaries
data(iebudgets)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(mycorpus, dictionary=mydict)
dictDfm

# simple lexical diversity measures
data(iebudgets)
finMins <- subset(iebudgets, no=="01")
finDfm <- dfm(finMins)
types <- rowSums(finDfm > 0)
tokens <- rowSums(finDfm)
ttrs <- types/tokens
plot(2008:2012, ttrs,
     ylim=c(.18,.25),   # set the y axis range
     type="b",          # points connected by lines
     xlab="Budget Year",
     ylab="Type/Token Ratios")

