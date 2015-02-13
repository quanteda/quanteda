###############
###############

#Lauderdale and Clark

# see formattingDocumentsForTopicModels
load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
txts <- unlist(Opinion_files[1])
names(txts) <- NULL


# 34070 documents, lets do some profiling tests...
library(quanteda)
library(tm)

txts <- txts[order(nchar(txts))]
numDocs <- 4000
start <- 13000
end <- start+numDocs
avgDocSizes <- c()
quantedaTimes <- c()
newTimes <- c()
newTimes <- c()
tmtimes <- c()
while(end < 32000){
    start <- end - numDocs
    print(start)
    # taking docs at a time from different points on the document length distibution
    thisTexts <- txts[start:end]
    avgDocSizes <- c(avgDocSizes, sum(nchar(thisTexts))/length(thisTexts) )
    print(avgDocSizes)
    
    sta <- proc.time()
    quCorp <- corpus(thisTexts)
    quDfm <- dfm(quCorp)
    sto <- proc.time()-sta
    quantedaTimes <- c(quantedaTimes, sto[1])
    
    
    
    sta <- proc.time()
    newCorp <- corpus(thisTexts)
    newCorp <- clean(newCorp)
    newDfm <- dfm(newCorp, clean=FALSE)
    sto <- proc.time()-sta
    newTimes <- c(newTimes, sto[1])
    
    
    
    sta <- proc.time()
    tmcorpus <- Corpus(VectorSource(thisTexts))
    tmdtm <- DocumentTermMatrix(tmcorpus,
                                control = list(stemming = FALSE, stopwords = FALSE, minWordLength = 0,
                                               removeNumbers = TRUE, removePunctuation = TRUE)) 
    sto <- proc.time()-sta
    tmtimes <- c(tmtimes, sto[1])
    
    
    start <- start + 1000
    end <- end + 1000
}


options(scipen=5)
library(ggplot2)
d1 <- data.frame(avgDocSizes, quantedaTimes, tmtimes, newTimes)
ggplot() + 
    geom_line(aes(avgDocSizes, quantedaTimes), d1) +  
    geom_line(aes(avgDocSizes, tmtimes ), d1)

library(reshape2)
d2 <- melt(d1, id=c('avgDocSizes'))
ggplot(d2, aes(x=avgDocSizes, y=value, colour=variable)) + 
    geom_line(aes(group=variable)) +
    geom_point(size=3)

