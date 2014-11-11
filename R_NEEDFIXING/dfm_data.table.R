
dfm_table <- function(x, ...) {
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


dfm_datatable_dcast <- function(x, ...) {
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
    
    alltokens <- reshape2::melt(alltokens, id=1:2, measure="V1")
    d <- reshape2::dcast(alltokens, docs ~ features, fun=sum)
    rownames(d) <- d[,1]
    d <- d[,-1]
    d <- as.matrix(d)
    dimnames(d) <- list(docs = names(tokenizedTexts), features = colnames(d))
    # restore original sort order
    d <- d[(1:nrow(d))[order(originalSortOrder)], , drop=FALSE]
    
    class(d) <- c("dfm", class(d))
    return(d)
}


library(quanteda)
system.time(t1 <- dfm_table(inaugTexts))
system.time(t2 <- dfm_datatable(inaugTexts))
system.time(t3 <- dfm_datatable_dcast(inaugTexts))



# References for speedup:
#
# http://smithlab.usc.edu/repos/methpipe/trunk/src/common-experimental/contingency-table.hpp
# http://smithlab.usc.edu/repos/methpipe/trunk/src/common-experimental/contingency-table.cpp
# http://pvanb.wordpress.com/2012/06/21/cross-tables-in-r-some-ways-to-do-it-faster/
# http://stackoverflow.com/questions/9171036/fastest-way-to-cross-tabulate-two-massive-logical-vectors-in-r
# http://www.mattblackwell.org/files/papers/bigdata.pdf
# http://stackoverflow.com/questions/6902087/proper-fastest-way-to-reshape-a-data-table
