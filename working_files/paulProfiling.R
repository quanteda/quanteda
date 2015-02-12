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
numDocs <- 300
start <- 20000
end <- start+numDocs
avgDocSizes <- c()
qutimes <- c()
tmtimes <- c()
while(end < 30000){
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
    qutimes <- c(qutimes, sto[1])
    
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
d1 <- data.frame(avgDocSizes, qutimes, tmtimes)
ggplot() + 
    geom_line(aes(avgDocSizes, qutimes), d1) +  
    geom_line(aes(avgDocSizes, tmtimes ), d1)

