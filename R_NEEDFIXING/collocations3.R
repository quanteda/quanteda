# Detect trigram collocations from text
# 
# Detects trigram collocations from texts or a corpus, returning a data.frame
# of collocations and their scores, sorted by the likelihood ratio \eqn{G^2}.
# @param x a text, a character vector of texts, or a corpus
# @param method association measure for detecting collocations.  Currently only
#    \code{lr} (likelihood ratio statistic \eqn{G^2}) is implemented.
# @param n length of the collocation.  Only trigrams (\code{n=3}) implemented so
#   far.
# @param top the number of collocations to return, sorted in descending order
#   of the requested statistic, or \eqn{G^2} if none is specified.
# @param ... additional parameters
# @return A data.frame of collocations, their frequencies, and the computed 
#   association measure.
# @export
# @author Kenneth Benoit
# @examples
# collocations(inaugTexts, top=10)
# collocations(inaugCorpus, top=10, method="chi2")
collocations3 <- function(x, ...) {
    UseMethod("collocations")
}

# @rdname collocations
# @export    
collocations3.character <- function(x, method=c("lr"), n=3, top=NULL, ...) {
    method <- match.arg(method)
    if (n != 3) stop("Hey, this method is for trigrams only! (n=3).")
    
    text <- clean(x, ...)
    t <- unlist(tokenize(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-2)], 
                            w2 = t[2:(length(t)-1)],
                            w3 = t[3:(length(t))],
                            count = 1)
    
    # set the data.table sort key
    #setkey(wordpairs, w1, w2, w3)
    
    ## counts of trigrams and bigrams
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2,w3"]
    setnames(wordpairsTable, "V1", "c123")
    
    # tabulate all w1 counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "c1")
    setkey(w1Table, w1)
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table]) # otherwise gives an encoding warning
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "c2")
    setkey(w2Table, w2)
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    
    # tabulate all w3 counts
    w3Table <- wordpairs[, sum(count), by=w3]
    setnames(w3Table, "V1", "c3")
    setkey(w3Table, w3)
    setkey(allTable2, w3)
    suppressWarnings(allTable3 <- allTable2[w3Table])
    
    # paired occurrence counts
    w12Table <- wordpairs[, sum(count), by="w1,w2"]
    setnames(w12Table, "V1", "c12")
    setkey(w12Table, w1, w2)
    setkey(allTable3, w1, w2)
    suppressWarnings(allTable4 <- allTable3[w12Table])
    
    w13Table <- wordpairs[, sum(count), by="w1,w3"]
    setnames(w13Table, "V1", "c13")
    setkey(w13Table, w1, w3)
    setkey(allTable4, w1, w3)
    suppressWarnings(allTable5 <- allTable4[w13Table])
    
    w23Table <- wordpairs[, sum(count), by="w2,w3"]
    setnames(w23Table, "V1", "c23")
    setkey(w23Table, w2, w3)
    setkey(allTable5, w2, w3)
    suppressWarnings(allTable6 <- allTable5[w23Table])
    
    ## cell counts
    # total table counts
    #N <- sum(allTable6$c123)  # total number of collocations (table N for all tables)
    N <- allTable3[, sum(c123)]
    
    # observed counts n_{ijk}
    allTable <- within(allTable6, {
        n111 <- c123
        n112 <- c12 - c123
        n121 <- c13 - c123
        n122 <- c1 - c12 - n121
        n211 <- c23 - c123
        n212 <- c2 - c12 - n211
        n221 <- c3 - c13 - n211
        n222 <- N - c1 - n211 - n212 - n221
    })
    
    #     ## testing from McInnes thesis Tables 19-20 example
    #     allTable <- rbind(allTable, allTable[478,])
    #     allTable$n111[479] <- 171
    #     allTable$n112[479] <- 3000
    #     allTable$n121[479] <- 2
    #     allTable$n122[479] <- 20805
    #     allTable$n211[479] <- 4
    #     allTable$n212[479] <- 2522
    #     allTable$n221[479] <- 7157
    #     allTable$n222[479] <- 88567875
    #     N <- 88601536
    
    # expected counts m_{ijk} for first independence model
    allTable <- within(allTable, {
        m1.111 <- (n111 + n121 + n112 + n122) * (n111 + n211 + n112 + n212) * (n111 + n211 + n121 + n221) / N^2
        m1.112 <- (n111 + n121 + n112 + n122) * (n111 + n211 + n112 + n212) * (n112 + n212 + n122 + n222) / N^2
        m1.121 <- (n111 + n121 + n112 + n122) * (n121 + n221 + n122 + n222) * (n111 + n211 + n121 + n221) / N^2
        m1.122 <- (n111 + n121 + n112 + n122) * (n121 + n221 + n122 + n222) * (n112 + n212 + n122 + n222) / N^2
        m1.211 <- (n211 + n221 + n212 + n222) * (n111 + n211 + n112 + n212) * (n111 + n211 + n121 + n221) / N^2
        m1.212 <- (n211 + n221 + n212 + n222) * (n111 + n211 + n112 + n212) * (n112 + n212 + n122 + n222) / N^2
        m1.221 <- (n211 + n221 + n212 + n222) * (n121 + n221 + n122 + n222) * (n111 + n211 + n121 + n221) / N^2
        m1.222 <- (n211 + n221 + n212 + n222) * (n121 + n221 + n122 + n222) * (n112 + n212 + n122 + n222) / N^2
    })
    
  
    epsilon <- .000000001  # to offset zero cell counts
    allTable <- within(allTable, lrratio <- 2 *
               ((n111 * log(n111 / m1.111 + epsilon)) + (n112 * log(n112 / m1.112 + epsilon)) +
                (n121 * log(n121 / m1.121 + epsilon)) + (n122 * log(n122 / m1.122 + epsilon)) +
                (n211 * log(n211 / m1.211 + epsilon)) + (n212 * log(n212 / m1.212 + epsilon)) +
                (n221 * log(n221 / m1.221 + epsilon)) + (n222 * log(n222 / m1.222 + epsilon))))         
               
    dt <- data.table(collocation = paste(allTable$w1, allTable$w2, allTable$w3),
                     count = allTable$c123,
                     G2 = allTable$lrratio) 
    setorder(dt, -G2)
    
    dt[1:ifelse(is.null(top), nrow(dt), top), ]
}

# library(quantedaData)
# data(exampleString)
# x <- exampleString
# method<-"lr"; top=NULL; n<-3
# text <- clean(x)


with(wordpairs, table(w1!="of", w2!="thousands", w3!="of", useNA="ifany"))
# , ,  = FALSE
# 
# 
#       FALSE TRUE
# FALSE     5    1
# TRUE      0   31
# 
# , ,  = TRUE
# 
# 
#       FALSE TRUE
# FALSE     0   31
# TRUE      1  421
with(wordpairs, table(w1!="of", w2!="thousands"))

#       FALSE TRUE
# FALSE     5   32
# TRUE      1  452
with(wordpairs, table(w2!="thousands", w3!="of"))

#       FALSE TRUE
# FALSE     5    1
# TRUE     32  452
> with(wordpairs, table(w1!="of", w3!="of"))

#       FALSE TRUE
# FALSE     6   31
# TRUE     31  422

allTable[425, ]
#    w1        w2 w3  c123 c1 c2 c3 c12 c13 c23 n222 n221 n212 n211 n122 n121 n112 n111
# 1: of thousands of     5 37  6 37   5   6   5  421   31    1    0   31    1    0    5

