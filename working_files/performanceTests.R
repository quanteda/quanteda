library(quanteda)
library(quantedaData)
library(ggplot2)
library(reshape2)
library(magrittr)


# c
tokenizeSingleCleanAfter <- function(s, sep=" ", clean=TRUE, ...) {
    # s <- unlist(s)
    tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
    if(clean) {
        tokens <- clean(tokens, ...)
    }
    return(tokens)
}

tokenizeSingleCleanFirst <- function(s, sep=" ", clean=TRUE, ...) {
    # s <- unlist(s)
    if(clean) {
        s <- clean(s, ...)
    }
    tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)

    return(tokens)
}



# tokenizing with optional cleaning, clean first
tokenizeCharvecCleanFirst <- function(texts, sep=" ", clean=TRUE, ...){
    # apply to each texts, return a list
    result <- lapply(texts, tokenizeSingleCleanFirst, sep, clean)
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    return(result)
}

# tokenizing with optional cleaning, clean first
tokenizeCharvecCleanAfter <- function(texts, sep=" ", clean=TRUE, ...){
    # apply to each texts, return a list
    result <- lapply(texts, tokenizeSingleCleanAfter, sep, clean)
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    return(result)
}

# tokenizing with optional cleaning, clean first
tokenizeNoClean1 <- function(texts, sep=" ", clean=FALSE, ...){
    # apply to each texts, return a list
    result <- lapply(texts, tokenizeSingleCleanAfter, sep, clean)
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    return(result)
}

# tokenizing with optional cleaning, clean first
tokenizeNoClean2 <- function(texts, sep=" ", clean=FALSE, ...){
    # apply to each texts, return a list
    result <- lapply(texts, tokenizeSingleCleanFirst, sep, clean)
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    return(result)
}




compareFunctions2 <- function(texts, funs,  splits=5, plot=TRUE, fnames=NULL) {
    funTimes <- as.data.frame(matrix())
    print(fnames)
    ind <- 1
    for(fun in funs){
        curFunTimes <- c() 
        numDocs <- c()
        print(length(texts))
        splitSize <- as.integer(length(texts)/splits)
        start <- 1
        end <- length(texts)
        while(start < end){
            curTexts <- texts[start:end]
            numDocs <- c(numDocs, length(curTexts))
            sta <- proc.time()
            r1 <-fun(curTexts)
            sto <- proc.time()-sta
            curFunTimes <- c(curFunTimes, sto[3])
            start <- start+splitSize
        }
       
        funTimes <- cbind(funTimes, curFunTimes)
        ind <- ind+1
        
    }
    if(is.null(fnames)){fnames <- paste("fun", 1:length(funs), sep="")}
    colnames(funTimes) <- c("numDocs", fnames)
    funTimes[,1] <- numDocs
    return(funTimes)
}





funcList <- c(tokenizeCharvecCleanAfter, tokenizeCharvecCleanFirst, quanteda::tokenize, quanteda::dfm, tokenizeNoClean1, tokenizeNoClean2)
funcNames <- c('cleanAfter', 'cleanFirst', 'original', 'defaultDfm', 'tokenizeNoClean1', 'tokenizeNoClean2')

timings <- compareFunctions2(inaugTexts,funcList , splits=5, fnames=funcNames) %>%
melt(id=c('numDocs'), value.name='elapsed') 

ggplot(timings, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line() +
    geom_point(size=3)




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






