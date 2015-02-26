library(quanteda)
library(quantedaData)
library(ggplot2)
library(reshape2)
library(magrittr)
library(profr)


###############
# definitions of functions to test
###############
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

library(tm)
# tm's dtm with cleaning
tmtest <- function(texts){
    tmcorpus <- Corpus(VectorSource(texts))
    tmdtm <- DocumentTermMatrix(tmcorpus,
                                control = list(stemming = FALSE, stopwords = FALSE, minWordLength = 0,
                                               removeNumbers = TRUE, removePunctuation = TRUE))
}





###############
# function for comparing performance of a list of functions on increasing numbers of texts
###############
compareFunctions <- function(texts, funs,  splits=5, plot=TRUE, fnames=NULL) {
    funTimes <- as.data.frame(matrix())
    print(fnames)
    ind <- 1
    for(fun in funs){
        curFunTimes <- c() 
        numDocs <- c()
        print(fnames[ind])
        splitSize <- as.integer(length(texts)/splits)
        start <- 1
        end <- length(texts)
        while(start < end){
            print(start)
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



###############
# inaugTexts
###############
d1 <- tokenizeCharvecCleanAfter(inaugTexts)
d2 <- tokenizeCharvecCleanFirst(inaugTexts)
identical(d1,d2)

funcList <- c(tokenizeCharvecCleanAfter, tokenizeCharvecCleanFirst, quanteda::tokenize, quanteda::dfm, tokenizeNoClean1,  tmtest)
funcNames <- c('cleanAfter', 'cleanFirst', 'originalTokenize', 'defaultDfm', 'tokenizeNoClean1',  'tmDfm')

timings <- compareFunctions(inaugTexts,funcList , splits=5, fnames=funcNames) %>%
melt(id=c('numDocs'), value.name='elapsed') 

ggplot(timings, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line() +
    geom_point(size=3)






###############
###############

#Lauderdale and Clark

###############
###############
# see formattingDocumentsForTopicModels
load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
txts <- unlist(Opinion_files[1])
txts <- txts[order(nchar(txts))]
# 34070 documents, lets do some profiling tests...



funcList <- c(tokenizeCharvecCleanAfter, tokenizeCharvecCleanFirst, quanteda::tokenize, quanteda::dfm, tokenizeNoClean1,  tmtest)
funcNames <- c('cleanAfter', 'cleanFirst', 'originalTokenize', 'defaultDfm', 'tokenizeNoClean1',  'tmDfm')

# The 1000 shortest texts
timings <- compareFunctions2(txts[1:1000],funcList , splits=5, fnames=funcNames) %>%
    melt(id=c('numDocs'), value.name='elapsed') 

ggplot(timings, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line() +
    geom_point(size=3)


#The 5 longest texts
longTxts <- txts[(length(txts)-5):length(txts)]
sum(nchar(longTxts))
timings <- compareFunctions(longTxts, funcList , splits=5, fnames=funcNames) %>%
    melt(id=c('numDocs'), value.name='elapsed') 

ggplot(timings, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line() +
    geom_point(size=3)

#The 8000 shortest texts
shortTexts <- txts[1:8000]
sum(nchar(shortTexts))
timings <- compareFunctions(txts[1:8000],funcList , splits=5, fnames=funcNames) %>%
    melt(id=c('numDocs'), value.name='elapsed') 

ggplot(timings, aes(x=numDocs, y=elapsed, colour=variable)) + 
    geom_line() +
    geom_point(size=3)



##=======================================##
# proper profiling
##=======================================##

library(profr)
p <- profr(dfm(inaugTexts))
plot(p)

p <- profr(dfm(shortTexts))
plot(p)

p <- profr(dfm(longTxts))
plot(p)

p <- profr(dfm(shortTexts, clean=FALSE))
plot(p)

p <- profr(dfm(longTxts, clean=FALSE))
plot(p)

p <- profr(dfm(shortTexts, matrixType="dense"))
plot(p)

p <- profr(dfm(longTxts, matrixType="dense"))
plot(p)





