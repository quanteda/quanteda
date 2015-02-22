library(quanteda)
library(quantedaData)
library(ggplot2)
library(reshape2)

options(scipen=5)

plotTimes <- function(times){
    
    d1 <- data.frame(avgDocSizes, quantedaTimes, tmtimes, newTimes)
    ggplot() + 
        geom_line(aes(avgDocSizes, quantedaTimes), d1) +  
        geom_line(aes(avgDocSizes, tmtimes ), d1)
    
    
    d2 <- melt(d1, id=c('avgDocSizes'))
    ggplot(d2, aes(x=avgDocSizes, y=value, colour=variable)) + 
        geom_line(aes(group=variable)) +
        geom_point(size=3)
    
}


compareFunctions <- function(texts, f1, f2, f3=NULL, splits=5) {
    f1Times <- c()
    f2Times <- c()
    f3Times <- c()
    splits <- seq(from=1, to=length(texts), by= as.integer(length(texts)/splits))
    splitSize = as.integer(length(texts)/splits)
    start=0
    end=length(texts)
    while(start < end){
        sta <- proc.time()
        r1 <-f1(inaugTexts)
        f1Times <- c(f1Times, proc.time()-sta)
        
        sta <- proc.time()
        r2 <-f2(inaugTexts)
        f2Times <- c(f2Times, proc.time()-sta)
        
        if(!is.null(f3)){
            sta <- proc.time()
            r3 <-f3(inaugTexts)
            f3Times <- c(f3Times, proc.time()-sta)
        }
    }
    
    return(c(f1=f1Times, f2=f2Times)
}

compareFunctions(inaugTexts)

# test harness for clean and tokenize

sta <- proc.time()
verify1 <- clean(tokenize(inaugTexts))
sto <- proc.time()-sta
print(sto)

sta <- proc.time()
verify2 <- tokenize(clean(inaugTexts))
sto <- proc.time()-sta
print(sto)


###############
###############


# movie reviews




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






