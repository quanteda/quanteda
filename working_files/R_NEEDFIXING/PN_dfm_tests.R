rm(list=ls())


dfm_table <- function(x, ...) {
    # name the vector if it is unnamed
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    textnames <- factor(names(x))
    
    # tokenizeTexts is a named list of tokens
    tokenizedTexts <- quanteda::tokenize(x, ...)
    # record original text order, since table() alphabetizes
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    # construct a "long" format data frame
    alltokens <- data.frame(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names=FALSE))
    # cross-tabulate
    d <- as.data.frame.matrix(table(alltokens$docs, alltokens$features))
    
    d <- as.matrix(d)
    dimnames(d) <- list(docs = rownames(tokenizedTexts), features = colnames(d))
    # restore original sort order
    d <- d[(1:nrow(d))[order(originalSortOrder)], , drop=FALSE]
    
    class(d) <- c("dfm", class(d))
    return(d)
}

dfm_table <- function(x, ...) {
    # name the vector if it is unnamed
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    textnames <- factor(names(x))
    
    # tokenizeTexts is a named list of tokens
    tokenizedTexts <- quanteda::tokenize(x, ...)
    # record original text order, since table() alphabetizes
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    # construct a "long" format data frame
    alltokens <- data.frame(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names=FALSE))
    # cross-tabulate
    d <- as.data.frame.matrix(table(alltokens$docs, alltokens$features))
    
    d <- as.matrix(d)
    dimnames(d) <- list(docs = rownames(tokenizedTexts), features = colnames(d))
    # restore original sort order
    d <- d[(1:nrow(d))[order(originalSortOrder)], , drop=FALSE]
    
    class(d) <- c("dfm", class(d))
    return(d)
}

dfm_datatable <- function(x, ...) {
    # name the vector if it is unnamed
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    textnames <- factor(names(x))
    
    # tokenizeTexts is a named list of tokens
    tokenizedTexts <- quanteda::tokenize(x, ...)
    # record original text order, since table() alphabetizes
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    # construct a "long" format data frame
    alltokens <- data.table(docs = rep(textnames, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names=FALSE))
    # make a vector of 1s
    alltokens[, n:=1L]
    setkey(alltokens, docs, features)
    # get sum within document
    alltokens <- alltokens[, by=list(docs,features), sum(n)]
    d <- tapply(alltokens$V1, list(alltokens$docs, alltokens$features), sum, rm.na=FALSE)
    d[which(is.na(d))] <- 0
    
    d <- as.matrix(d)
    dimnames(d) <- list(docs = rownames(tokenizedTexts), features = colnames(d))
    # restore original sort order
    d <- d[(1:nrow(d))[order(originalSortOrder)], , drop=FALSE]
    
    class(d) <- c("dfm", class(d))
    return(d)
}




dfm_tapplyonly <- function(x, ...) {
    # name the vector if it is unnamed
    if (is.null(names(x))) {
        names(x) <- factor(paste("text", 1:length(x), sep=""))
    }
    textnames <- factor(names(x))
    
    # tokenizeTexts is a named list of tokens
    tokenizedTexts <- quanteda::tokenize(x, ...)
    # record original text order, since table() alphabetizes
    originalSortOrder <- (1:length(tokenizedTexts))[order(names(tokenizedTexts))]
    # construct a "long" format data frame
    alltokens <- data.frame(docs = rep(names(tokenizedTexts), sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts, use.names=FALSE),
                            n=1L)    # make a vector of 1s
    d <- tapply(alltokens$n, list(alltokens$docs, alltokens$features), sum, rm.na=FALSE)
    d[which(is.na(d))] <- 0
    dimnames(d) <- list(docs = names(tokenizedTexts), features = colnames(d))
    # restore original sort order
    d <- d[(1:nrow(d))[order(originalSortOrder)], , drop=FALSE]
    
    class(d) <- c("dfm", class(d))
    return(d)
}

dfm_tm <- function(x,...){
    tmcorpus <- Corpus(VectorSource(x))
    tmdtm <- DocumentTermMatrix(tmcorpus,
                                control = list(stemming = FALSE, stopwords = FALSE, minWordLength = 0,
                                               removeNumbers = FALSE, removePunctuation = FALSE)) 
}

gc()


load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
txts <- unlist(Opinion_files[1])
names(txts) <- NULL
txts <- txts[order(nchar(txts))]

sampDocs <- sample(txts[23000:33000],7000)

gc()
library(quanteda)
library(tm)
system.time(t0 <- dfm(sampDocs))
gc()
system.time(t1 <- dfm_table(sampDocs))
gc()
system.time(t2 <- dfm_datatable(sampDocs))
gc()
system.time(t3 <- dfm_tapplyonly(sampDocs))
gc()
system.time(t4 <- dfm_tm(sampDocs))
gc()
system.time(t5 <- dfm(sampDocs, removePunct=FALSE, removeDigits=FALSE))
gc()



Rprof()
dfm(sampDocs)
Rprof(NULL)
t0p <- summaryRprof()
t0p
