library(quanteda)
library(quantedaData)
library(ggplot2)
library(reshape2)


tokenize1 <- function(texts){
    return(quanteda::tokenize(texts))
}

tokenize2 <- function(texts, sep=" "){
    tokenizeSingle <- function(s, sep=" ") {
        
        # s <- unlist(s)
        tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
        #tokens <- clean(tokens, ...)
        return(tokens)
    }
    
    # apply to each texts, return a list
    result <- lapply(texts, tokenizeSingle, sep)
    
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    
    return(result)
}



compareFunctions <- function(texts, f1, f2,  splits=5, plot=TRUE) {
    f1Times <- c()
    f2Times <- c()
    numDocs <- c()
    splitSize <- as.integer(length(texts)/splits)
    start <- 1
    end <- length(texts)
    while(start < end){
        curTexts <- texts[start:end]
        numDocs <- c(numDocs, length(curTexts))
        sta <- proc.time()
        r1 <-f1(curTexts)
        sto <- proc.time()-sta
        f1Times <- c(f1Times, sto[3])
        sta <- proc.time()
        r2 <- f2(curTexts)
        sto <- proc.time()-sta
        f2Times <- c(f2Times, sto[3])
        start <- start+splitSize
    }
    times <- data.frame(f1=f1Times, f2=f2Times, numDocs=numDocs)
    plt <- ggplot(melt(times, id=c('numDocs'), value.name='elapsed'), 
           aes(x=numDocs, y=elapsed, colour=variable)) + 
        geom_line(aes(group=variable)) +
        geom_point(size=3)
    return(plt)
}
gp <- compareFunctions(inaugTexts, tokenize1, tokenize2)
gp
# plot timings against number of doucments


ggplot() + 
    geom_line(aes(numDocs, f1), res) +  
    geom_line(aes(numDocs, f2), res)


d2 <- melt(res, id=c('numDocs'), value.name='elapsed')
ggplot(d2, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line(aes(group=variable)) +
    geom_point(size=3)

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






